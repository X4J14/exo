(in-package :cl-user)

(format t "
;;;;
; Exo (dependency managment tool (DMT) for Common Lisp)
;
; Source code, issues, documentation:
;  https://codeberg.org/x4j14/exo
;  https://github.com/x4j14/exo
;
; Modules repository:
;  https://codeberg.org/x4j14/exo-prime-repo
;  https://github.com/x4j14/exo-prime-repo
;
; To get available commands please type (exo:help).
;
; THIS SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THIS
; SOFTWARE OR THE USE OR OTHER DEALINGS IN THIS SOFTWARE.
;
; Runtime: ~s ~s
;;;;~%" (lisp-implementation-type) (lisp-implementation-version))

(require :uiop)
(require :sb-posix)

(provide :exo)

;(pushnew :exo-debug *features*)

#+exo-debug
(defpackage #:exo-debug
	(:export
		 #:list-packages
		 #:*loaded-packages*))
#+exo-debug
(defparameter exo-debug:*loaded-packages* ())

(defpackage #:exo
	(:import-from #:cl
		#:defun
		#:defpackage
		#:in-package)
	(:export
		#:exo
		#:repo
		#:clone
		#:list
		#:search
		#:verify
		#:clean
		#:install
		#:remove
		#:token
		#:sign
		#:sign-verify
		#:run
		#:suppress-output
		#:check-utils
		#:interactive
		#:version
		#:help))
(in-package #:exo)

(defun version () 1.0)

; Package for defining necessary
; temporary variables at runtime
(defpackage #:exo-tmp)

;;;;;;;;;;;;;;;;;;;;
;;;   exo util   ;;;
;;;;;;;;;;;;;;;;;;;;

(defpackage #:exo-utils
	(:use #:cl)
	(:shadow
		#:debug
		#:read-from-string
		#:write-to-string
		#:string-downcase
		#:string-upcase)
	(:export
		#:lets
		#:let-if
		#:let-unless
		#:let-when
		#:let-loop
		#:let-bind
		#:pfmt
		#:pfmtl
		#:fmt
		#:fmtl
		#:without-output
		#:string+
		#:string-downcase
		#:string-downcase+
		#:string-upcase
		#:string-upcase+
		#:string-starts-with
		#:string-ends-with
		#:iterate-by
		#:length+1
		#:length-1
		#:booleanp
		#:read-without-echo
		#:members
		#:make-keyword
		#:make-random-string
		#:ensure-length
		#:read-from-string
		#:write-to-string
		#:package-symbols
		#:list-packages))
(in-package #:exo-utils)

(defmacro debug (string &optional &rest args)
	(when (find-symbol "DEBUG-ON" *package*)
		`(format t (string+ "~%" ,string) ,@args)))

;;; let ;;;

(defmacro lets (vars &body body)
	`(let* (,@(labels ((to-pairs (l1 &optional l2)
			(if l1
				(to-pairs
					(cddr l1)
					(append l2 (list (list (car l1) (cadr l1)))))
				l2)))
			(to-pairs vars)))
		,@body
	))

(defmacro let-if (vars cond then &optional else)
	`(lets ,vars (if ,cond ,then ,else)))

(defmacro let-unless (vars cond &body body)
	`(lets ,vars (unless ,cond ,@body)))

(defmacro let-when (vars cond &body body)
	`(lets ,vars (when ,cond ,@body)))

(defmacro let-loop (vars &body body)
	`(lets ,vars (loop ,@body)))

(defmacro let-bind (vars value-form &body body)
	`(multiple-value-bind ,vars ,value-form ,@body))

;;; format ;;;

(defparameter *is-suppress-output* ())

(defmacro fmt (string &optional &rest args)
	`(format nil ,string ,@args))

(defmacro fmtl (string &optional &rest args)
	`(progn
		(format nil ,string ,@args)
		(terpri)))

(defmacro pfmt (string &optional &rest args)
	`(unless *is-suppress-output*
		(format t ,string ,@args)))

(defmacro pfmtl (string &optional &rest args)
	`(unless *is-suppress-output*
		(format t ,string ,@args)
		(terpri)))

(defun exo:suppress-output (&optional (value nil value-p))
	"Disable output for pfmtl, pfmt"
	(check-type value (or boolean))
	(if value-p
		(setf *is-suppress-output* value)
		*is-suppress-output*))

(defmacro without-output (&body body)
	`(unless *is-suppress-output*
		(exo:suppress-output t)
		(unwind-protect ,@body
			(exo:suppress-output nil))))

;;; error ;;;

(defmacro when-error (cond string &rest args)
	`(when ,cond (error ,string ,@args)))

;;; macro characters ;;;

(defun question-mark-print (stream char)
	(declare (ignore char))
	(list 'print (read stream t nil t)))

;(set-macro-character #\? #'question-mark-print)

;;; misc ;;;

(declaim (inline length-1))
(defun length-1 (seq)
	(1- (length seq)))

(declaim (inline length+1))
(defun length+1 (seq)
	(1+ (length seq)))

(defvar %new-random-state% (make-random-state t))
(defun make-random-string (length)
	(with-output-to-string (s)
		(loop repeat length do
			(write-char (code-char (+ 48 (random 10 %new-random-state%))) s)
		)))

(declaim (inline booleanp))
(defun booleanp (value)
	(typep value 'boolean))

(defun suppress-echo (val)
	(check-type val boolean)
	(lets (attr (sb-posix:tcgetattr sb-sys:*tty*))
		(setf (sb-posix:termios-lflag attr)
			(funcall
				(if val #'logandc2 #'logior)
				(sb-posix:termios-lflag attr) sb-posix:echo))
		(sb-posix:tcsetattr sb-sys:*tty* sb-posix:tcsanow attr)
	))

(defun read-without-echo ()
	(suppress-echo t)
	(unwind-protect
		(progn
			(clear-input sb-sys:*tty*)
			(read-line sb-sys:*tty*))
	(suppress-echo nil)))

;;; list ;;;

(defun members (list items mode)
	(check-type list list)
	(check-type items list)
	(check-type mode (member :or :and))
	(case mode
		(:or
			(dolist (item items)
				(let-when (res (member item list)) res
					(return res))))
		(:and
			(loop for item in items do
				(let-when (res (member item list)) (not res)
					(return nil))
				:finally (return t)
			))))

(defun iterate-by (list num lambda)
	(check-type num integer)
	(check-type list list)
	(check-type lambda function)
	(unless (zerop (mod (length list) num))
		(error "Number of elements (~d) must be a multiple of ~d" (length list) num))
	(loop while (> (length list) 0) do
		(apply lambda (loop repeat num collect (pop list)))
	))

;;; sequence ;;;

(defun ensure-length (seq min max error-msg &optional &key (return :length))
	(check-type return (member :length :sequence))
	(lets (l (length seq))
		(when (or (< l min) (> l max))
			(error error-msg))
		(case return
			(:length l)
			(:sequence seq))
	))

;;; string ;;;

; Prevent code evaluation
(declaim (inline read-from-string))
(defun read-from-string (data)
	;(with-standard-io-syntax
		(lets (cl:*read-eval* nil)
			(cl:read-from-string data))
	);)

;; (cl:write-to-string) returns "NIL" for nil value
;; need to avoid this issue
(defun write-to-string (value)
	(when value
		(cl:write-to-string value)
	))

;; (cl:string-downcase) returns "nil" string for nil value
;; need to avoid this issue
#| Style warning: The binding of STRING is not a STRING: NIL
(declaim (inline string-downcase))
(defun string-downcase (value)
	(declare (type (or string null) value))
	(when value
		(cl:string-downcase value)
	))
|#

(defmacro string-downcase (value)
	`(when ,value
		(cl:string-downcase ,value)
	))

(defmacro string-downcase+ (&rest strings)
	`(string-downcase (concatenate 'string ,@strings)))

(defmacro string+ (&rest strings)
	`(concatenate 'string ,@strings))

(defmacro string-upcase (value)
	`(when ,value
		(cl:string-upcase ,value)
	))

(defmacro string-upcase+ (&rest strings)
	`(string-upcase (concatenate 'string ,@strings)))

(declaim (inline string-starts-with))
(defun string-starts-with (string start)
	;(declare (type string string start))
	(when (>= (length string) (length start))
		(equal (subseq string 0 (length start)) start)
	))

(declaim (inline string-ends-with))
(defun string-ends-with (string end)
	;(declare (type string string end))
	(when (>= (length string) (length end))
		(equal (subseq string (- (length string) (length end))) end)
	))

(defun string-cut (string val)
	(check-type string string)
	(check-type val number)
	(when (or
				(and (> val 0) (> val (length string)))
				(and (< val 0) (< val (- (length string)))))
		(error "Cut value ~s is out of string length (~d)" val (length string)))
	(cond
		((> val 0) (subseq string val))
		((< val 0) (subseq string 0 (+ (length string) val)))
		(t string)
	))

(defun package-symbols (package-symbol &optional (mode :external))
	"Get the package symbols. Allowed modes: :accessible, :external, :all"
	(check-type mode (member :accessible :external :all))
	(let (lst)
		(case mode
			(:accessible
				(do-symbols (s (find-package package-symbol)) (push s lst)))
			(:external
				(do-external-symbols (s (find-package package-symbol)) (push s lst)))
			(:all
				(do-all-symbols (s lst)
					(when (eq (find-package package-symbol) (symbol-package s)) (push s lst)))))
		lst
	))

; "note: deleting unreachable code" with inline
;(declaim (inline make-keyword))
(defun make-keyword (name)
	(values (intern (string-upcase name) :keyword)))

;;;;;;;;;;;;;;;;;;
;;;   exo fs   ;;;
;;;;;;;;;;;;;;;;;;

(defpackage #:exo-fs
	(:use #:cl #:exo-utils)
	(:shadow
		#:read-from-string
		#:write-to-string
		#:string-downcase
		#:string-upcase)
	(:export
		#:make-file-link
		#:.path
		#:.name
		#:.parent
		#:file-read-content
		#:file-write-content
		#:file-iterate-lines
		#:directory-content
		#:directory-content-deep
		#:directory-copy-content
		#:directory-delete-content
		#:directory-delete))
(in-package #:exo-fs)

(defun trim-last-slash (string)
	(if (string-ends-with string "/")
		(subseq string 0 (length-1 string))
		string
	))

;;; class file link ;;;

(defclass <file-link> () (
	(pathname :initarg :pathname :reader .pathname)
	(path :initform nil)
	(parent :initform nil)
	(name :initform nil)
	(is-file :initform nil)
	(is-hidden :initform nil)
))

(defmethod .path ((obj <file-link>))
	(with-slots (path) obj
		(if path path
			(setf (slot-value obj 'path)
				(namestring (.pathname obj)))
		)))

(defmethod .name ((obj <file-link>))
	(with-slots (name) obj
		(if name name
			(setf (slot-value obj 'name)
				(if (equal (.path obj) "/")
					""
					(lets (path (trim-last-slash (.path obj)))
						(if (pathname-type path)
							(string+ (pathname-name path) "." (pathname-type path))
							(pathname-name path))
					))))))

(defmethod .is-file ((obj <file-link>))
	(with-slots (is-file) obj
		(if is-file is-file
			(setf (slot-value obj 'is-file)
				(pathname-name (slot-value obj 'pathname)))
		)))

(defmethod .is-dir ((obj <file-link>))
	(not (.is-file obj)))

 (defmethod .parent ((obj <file-link>))
	(with-slots (parent) obj
		(if parent parent
			(setf (slot-value obj 'parent)
				(directory-namestring
					(if (.is-file obj)
						(.pathname obj)
						(trim-last-slash (.path obj))
					))))))

(defmethod .is-hidden ((obj <file-link>))
	(with-slots (is-hidden) obj
		(if is-hidden is-hidden
			(setf (slot-value obj 'is-hidden)
				(string-starts-with (.name obj) "."))
		)))

(defmethod print-object ((obj <file-link>) out)
	(print-unreadable-object (obj out :type t)
		(format out "~%  path: ~s" (.path obj))
		(format out "~%  parent: ~s" (.parent obj))
		(format out "~%  name: ~s" (.name obj))
		(format out "~%  type: ~a" (if (.is-file obj) "file" "dir"))
		(format out "~%  hidden: ~a~%" (if (.is-hidden obj) "yes" "no"))
	))

(defun make-file-link (path &optional ensure)
	(check-type path (or string pathname))
	(check-type ensure (member nil :ensure-file :ensure-dir))
	(when (or (equal path "") (eq path #p""))
		(error "Invalid path: empty string"))
	(lets (pname (probe-file path))
		(unless pname
			(error "Path does not exist: ~s" path))
		(case ensure
			(:ensure-file
				(unless (pathname-name pname)
					(error "Path is not a file: ~s" path)))
			(:ensure-dir
				(when (pathname-name pname)
					(error "Path is not a directory: ~s" path))))
		(make-instance '<file-link> :pathname pname)
	))

;;; file content ;;;

(defun file-read-content (file-path)
	(with-open-file (in file-path)
		(lets (out (make-string-output-stream))
			(do ((c (read-char in) (read-char in nil 'the-end)))
				((not (characterp c)))
				(write-string (make-string 1 :initial-element c) out))
			(close in)
			(get-output-stream-string out)
		)))

(defun file-write-content (file-path &rest content)
	(lets (stream (open file-path :direction :output :if-exists :supersede))
		(dolist (c content)
			(if (stringp c)
				(write-string c stream)
				(write-sequence c stream)))
		(close stream)
	))

(defun file-iterate-lines (file-path fn)
	(with-open-file (in file-path)
		(loop
			(multiple-value-bind (line eof) (read-line in nil)
				(funcall fn line)
				(when eof
					(return-from file-iterate-lines)
				)))))

;;; directory content ;;;

(defun directory-content (path &rest filter)
	(check-type path (or string pathname))
	;(check-type filter (member :file :hfile :dir :hdir)) ; todo
	(lets (
			dir (make-file-link path :ensure-dir)
			pnames (directory (string+ (.path dir) "*.*")) ; maybe use uiop is better
			result ())
		(dolist (pname pnames)
			(lets (file (make-file-link pname))
				(when (or (not filter) (or
						(when (member :file filter)
							(and (.is-file file) (not (.is-hidden file))))
						(when (member :hfile filter)
							(and (.is-file file) (.is-hidden file)))
						(when (member :dir filter)
							(and (.is-dir file) (not (.is-hidden file))))
						(when (member :hdir filter)
							(and (.is-dir file) (.is-hidden file)))))
					(push pname result))))
		(reverse result)
	))

(defun directory-content-deep (path &rest filter)
	(lets (result ())
		(labels ((iterate-dir (path filter)
			(dolist (pname (apply #'directory-content path filter))
				(if (pathname-name pname) ; is ordinary file
					(push pname result)
					(iterate-dir pname filter)))))
			(iterate-dir path filter))
		(reverse result)
	))

(defun directory-copy-content (src-path dst-path &rest filter)
	(check-type src-path (or string pathname))
	(check-type dst-path (or string pathname))
	(lets (dst-dir (make-file-link dst-path :ensure-dir))
		(dolist (pname (apply #'directory-content src-path filter))
			(lets (file (make-file-link pname))
				(if (.is-file file)
					(uiop:copy-file (.path file) (string+ (.path dst-dir) (.name file)))
					(directory-copy-content
						(.path file)
						(ensure-directories-exist (string+ (.path dst-dir) (.name file) "/")))
				)))))

(defun directory-delete-content (path &rest filter)
	(check-type path (or string pathname))
	(lets (files (apply #'directory-content path filter))
		(dolist (file files)
			;(if (.is-file (make-file-link file))
			(if (uiop:file-pathname-p file)
				(delete-file file)
				(uiop:delete-directory-tree file :validate t)))
		files
	))

(defun directory-delete (path)
	(check-type path (or string pathname))
	(directory-delete-content path)
	(uiop:delete-empty-directory path))

;;;;;;;;;;;;;;;;;;;;;
;;;   exo plist   ;;;
;;;;;;;;;;;;;;;;;;;;;

(defpackage #:exo-plist
	(:use #:cl #:exo-fs)
	(:import-from #:exo-utils
		#:lets
		#:let-if)
	(:shadowing-import-from #:exo-utils
		#:read-from-string)
	(:export
		#:plist/iterate
		#:plist/set
		#:plist/get
		#:plist/find
		#:plist/read
		#:plist/ensure
		#:plist/ensure-unique-keys
		#:plist/check))
(in-package #:exo-plist)

(declaim (inline plist/iterate))
(defun plist/iterate (plist fn)
	(loop :for (key value) :on plist :by #'cddr :while key :do
		(funcall fn key value)
	))

(declaim (inline plist/get))
(defun plist/get (plist &rest keys)
	(declare (type list plist keys))
	(lets (
			val (getf plist (first keys))
			keys (rest keys))
		(if (and val (listp val) keys)
			(apply #'plist/get val keys)
			val
		)))

(defun plist/get-def (plist default &rest keys)
	(let-if (result (funcall #'plist/get plist keys)) result
		result
		default
	))

(defun plist/find (plist key &optional value)
	(check-type plist list)
	(check-type key keyword)
	(check-type value (or null string))
	(dolist (prop plist)
		(if value
			(when (equal (plist/get prop key) value)
				(return prop))
			(when (plist/get prop key)
				(return prop))
		)))

(declaim (inline plist/read))
(defun plist/read (file-path)
	"Convert content to plist"
	(read-from-string (file-read-content file-path)))

(defmacro plist/set (plist &rest props)
	;(declare (type list plist props))
	`(plist/iterate (list ,@props)
		(lambda (key value)
			(setf (getf ,plist key) value)
		)))

(declaim (inline plist/ensure))
(defun plist/ensure (plist &rest props)
	(dolist (prop props)
		(unless (plist/get plist prop)
			(error "Property ~(~s~) is not defined" prop))
	))

(defun plist/ensure-unique-keys (plist &aux lst)
	(plist/iterate plist (lambda (key value)
		(declare (ignore value))
		(if (find key lst)
			(error "Duplicated key ~(~s~) in the property list" key)
			(push key lst)
		))))

(defun plist/check (plist test-plist &optional check-fn)
	(plist/iterate plist (lambda (key value)
		(lets (test-prop (plist/find test-plist key))
			(unless (or test-prop (and check-fn (funcall check-fn key)))
				(error "Unknown property ~(~s~)" key))
			(when test-prop
				(unless (funcall (symbol-function (second test-prop)) value)
					(error "Invalid property ~(~s~). Value type is not ~(~s~)"
						key (second test-prop))))
		)))
	t)

;;;;;;;;;;;;;;;;;;;;
;;;   exo json   ;;;
;;;;;;;;;;;;;;;;;;;;

(defpackage #:exo-json
	(:use #:cl #:exo-utils)
	(:import-from #:exo-fs
		 #:file-read-content)
	(:shadowing-import-from #:exo-utils
		#:debug
		#:string-downcase
		#:string-upcase
		#:read-from-string
		#:write-to-string)
	(:export #:read-json))
(in-package #:exo-json)

;(intern "DEBUG-ON")
(debug "Debug ~s" (package-name *package*))

(defvar %markup-chars% '(#\space #\tab #\return #\linefeed))

(declaim (inline alpha-lower-case-p))
(defun alpha-lower-case-p (char)
	(and (alpha-char-p char) (lower-case-p char)))

;; any char except %markup-chars%
(declaim (inline skip-to-data-char))
(defun skip-to-data-char (in)
	(do ((c (read-char in) (read-char in nil 'the-end))) ((not (characterp c)))
		(unless (member c %markup-chars%)
			(return c))
	))

(declaim (inline skip-to-char))
(defun skip-to-char (in char &optional &key stop-char)
	(do ((c (read-char in) (read-char in nil 'the-end))) ((not (characterp c)))
		(when (eq c stop-char)
			(return c))
		(when (eq c char)
			(return))
	))

(declaim (inline read-to-char))
(defun read-to-char (in char &optional &key stop-char)
	(with-output-to-string (s)
		(do ((c (read-char in) (read-char in nil 'the-end))) ((not (characterp c)))
			(when (eq c stop-char)
				(return c))
			(if (eq c char)
				(return)
				(write-char c s)))
	))

(declaim (inline read-to-chars))
(defun read-to-chars (in stop-chars &optional do-unread-chars)
	(with-output-to-string (s)
		(do ((c (read-char in) (read-char in nil 'the-end))) ((not (characterp c)))
			(if (member c stop-chars)
				(progn
					(when (member c do-unread-chars)
						(unread-char c in))
					(return))
				(write-char c s)
			))))

(defclass <json-reader> () (
	(in) (out)
	(array-start-char :initform #\[)
	(array-end-char :initform #\])
	(object-start-char :initform #\{)
	(object-end-char :initform #\})
	(comma-char :initform #\,)
	(markup-chars :initform '(#\space #\tab #\return #\linefeed))
	(read-value-stop-chars :initform (append '(#\, #\} #\]) %markup-chars%))
	(read-value-do-unread-chars :initform (list #\] #\}))
))

(defun make-json-reader (data data-type)
	(check-type data string)
	(check-type data-type (member :file :string))
	(lets (
			json (case data-type
				(:file (file-read-content data))
				(:string data))
			reader (make-instance '<json-reader>))
		(setf
			(slot-value reader 'in) (make-string-input-stream json)
			(slot-value reader 'out) (make-string-output-stream))
		reader
	))

(defgeneric .read-property (obj))
(defgeneric .read-value (obj &optional is-append-space))

(defmethod .read ((obj <json-reader>) &optional context)
	;(check-type context (member nil :array :object))
	(with-slots (in out
		array-start-char array-end-char
		object-start-char object-end-char
		comma-char markup-chars) obj
		(do ((c (read-char in) (read-char in nil 'the-end))) ((not (characterp c)))
			(unless (or (member c markup-chars) (eq c comma-char))
				(debug "~a" c)
				(cond
					((eq c array-start-char)
						(debug "Start array")
						(write-char #\( out)
						(.read obj :array))
					((eq c array-end-char)
						(debug "End array")
						(write-char #\) out)
						(return))
					((eq c object-start-char)
						(debug "Start object")
						(write-char #\( out)
						(.read obj :object))
					((eq c object-end-char)
						(debug "End object")
						(write-char #\) out)
						(return))
					((eq context :array)
						(debug "Read array value")
						(unread-char c in)
						(.read-value obj t))
					((eq context :object)
						(debug "Read properties")
						(unread-char c in)
						(loop while (.read-property obj)))
					(t (error "Unexpected char: ~s" c))
				)))))

;: "name" : "text", | (true | false), | 1, | null (, | })
(defmethod .read-property ((obj <json-reader>))
	(debug "Read property")
	(with-slots (in out
		object-start-char object-end-char) obj
	(lets (
			stop-char (skip-to-char in #\"
				:stop-char object-end-char))
		(debug "stop char >~s<" stop-char)1
		(if stop-char
			(progn
				(debug "Stop!")
				(unread-char stop-char in)
				nil)
			(lets (prop-name (read-to-char in #\"))
				(skip-to-char in #\:)
				(write-char #\space out)
				(write-char #\: out)
				(write-string prop-name out)
				(write-char #\space out)
				(debug "Read property value: ~s" prop-name)
				(.read-value obj)
				t
			)))))

(defmethod .read-value ((obj <json-reader>) &optional is-append-space)
	(with-slots (in out
		array-start-char array-end-char
		object-start-char object-end-char
		read-value-stop-chars
		read-value-do-unread-chars) obj
		(lets (data-char (skip-to-data-char in))
			(debug "data-char >~a<" data-char)
			(cond
				((eq data-char #\{)
					(progn
						(debug "Start object while read value")
						(write-char #\( out)
						(.read obj :object)))
				((eq data-char #\[)
					(progn
						(debug "Start array while read value")
						(write-char #\( out)
						(.read obj :array)))
				((eq data-char #\")
					(progn
						(when is-append-space (write-char #\space out))
						(write-char #\" out)
						(write-string (read-to-char in #\") out)
						(write-char #\" out)))
				((digit-char-p data-char)
					(progn
						(when is-append-space (write-char #\space out))
						(write-char data-char out)
						(write-string
							(read-to-chars in
								read-value-stop-chars
								read-value-do-unread-chars) out)))
				((alpha-lower-case-p data-char)
					(lets (value (progn
										(unread-char data-char in)
										(read-to-chars in read-value-stop-chars
											read-value-do-unread-chars)))
						(when is-append-space (write-char #\space out))
						(cond
							((equal value "null") (write-string "nil" out))
							((equal value "true") (write-char #\t out))
							((equal value "false") (write-string "nil" out))
							(t (error "Unexpected value: ~s" value)))
					))))))

(declaim (inline read-string-value))
(defun read-string-value (in)
	(with-output-to-string (s)
		(let (prev-char)
			(do ((c (read-char in) (read-char in nil 'the-end))) ((not (characterp c)))
				(if (eq c #\")
					(if (eq prev-char #\\)
						(write-char c s)
						(return s))
					(write-char c s))
				(setf prev-char c)
			))))

(defun read-json (data data-type)
	(check-type data string)
	(check-type data-type (member :file :string))
	(lets (
			reader (make-json-reader data data-type)
			in (slot-value reader 'in)
			out (slot-value reader 'out))
		(.read reader)
		(close in)
		(prog1
			(read-from-string (get-output-stream-string out))
			(close out)
		)))

;;;;;;;;;;;;;;;;;;;;;
;;;   exo fetch   ;;;
;;;;;;;;;;;;;;;;;;;;;

(defpackage #:exo-fetch
	(:use #:cl #:exo-fs #:exo-plist #:exo-json)
	(:import-from #:exo-utils
		#:lets
		#:string+
		#:pfmt
		#:pfmtl)
	(:shadowing-import-from #:exo-utils #:debug)
	(:export
		#:<file-validator>
		#:.validate
		#:fetch-file
		#:fetch-data
		#:fetch-json-data))
(in-package #:exo-fetch)

(defconstant +options+ " --compressed --max-filesize 250K ")

(intern "DEBUG-OFF")
(debug "Debug ~s" (package-name *package*))

(declaim (inline headers-to-string))
(defun headers-to-string (headers)
	(when headers
		(with-output-to-string (s)
			(dolist (h headers)
				(when h
					(write-string " -H \"" s)
					(write-string (first h) s)
					(write-string ": " s)
					(write-string (rest h) s)
					(write-char #\" s))
			))))

;;; class file validator ;;;

(defclass <file-validator> ()())
(defgeneric .validate (validator file-path file-hash error-msg))

(defun fetch-file (dst-dir file-name url &optional headers file-hash file-validator)
	(check-type url string)
	(check-type headers (or null list))
	(check-type file-hash (or null string))
	(check-type file-validator (or null <file-validator>))
	(pfmtl "Fetch file: ~s" url)
	(lets (
			headers (headers-to-string headers)
			file-path (string+ dst-dir file-name)
			cmd (string+ "curl -L -o " file-name " -s --output-dir " dst-dir headers +options+ url))
		(debug "~s" cmd)
		(uiop:run-program cmd :error-output *standard-output*)
		(lets (file (make-file-link file-path :ensure-file))
			(pfmtl "File fetched: ~s" (.path file))
			(when (and file-hash file-validator)
				(pfmtl "Validate hash: ~s" file-hash)
				(.validate file-validator file-path file-hash
					"Error fetch file. Invalid hash for: ~s. Valid: ~s, calculated: ~s")
				(pfmt " - OK")
			))
			file-path
		))

(defun fetch-data (url &optional headers)
	(check-type url string)
	(check-type headers (or null list))
	(pfmtl "Fetch data: ~s" url)
	(lets (
			output (make-string-output-stream)
			cmd (string+ "curl -L -s " (headers-to-string headers) +options+ url))
		(debug "~s" cmd)
		(uiop:run-program cmd :output output
			:error-output *standard-output*)
		(get-output-stream-string output)
	))

(defun fetch-json-data (url &optional headers)
	(lets (props (read-json (fetch-data url headers) :string))
		(debug "~a" props)
		(when (and (keywordp (first props)) (plist/get props :message))
			(error "Error fetch data: ~s" props))
		props
	))

;;;;;;;;;;;;;;;;;;;;;
;;;   exo error   ;;;
;;;;;;;;;;;;;;;;;;;;;

(defpackage #:exo-error
	(:use #:cl)
	(:import-from #:exo-utils
		#:fmt #:pfmtl)
	(:export
		#:<exo-error>
		#:<exo-cfg-error>
		#:exo-error
		#:exo-cfg-error
		#:.message
		#:.print))
(in-package #:exo-error)

(define-condition <exo-error> (error) (
	(message-prefix :initform "Operation failed:" :reader .message-prefix)
	(message :initarg :message :initform nil :reader .message))
	(:report (lambda (cond stream)
		(format stream "~a ~a" (.message-prefix cond) (.message cond))
	)))

(defmethod .print ((obj <exo-error>))
	(pfmtl "~a" obj))

(define-condition <exo-cfg-error> (<exo-error>) (
	(cfg-path :initarg :cfg-path :initform nil :reader .cfg-path))
	(:report (lambda (cond stream)
		(format stream "~a Error read config: ~s. ~a"
			(.message-prefix cond) (.cfg-path cond) (.message cond))
	)))

(defparameter *is-interactive* t)

(declaim (inline exo:interactive))
(defun exo:interactive (&optional (value nil valuep))
	(check-type value boolean)
	(if valuep
		(setf *is-interactive* value)
		*is-interactive*
	))

(defmacro exo-error (msg &rest args)
	`(if *is-interactive*
		(error '<exo-error> :message (fmt ,msg ,@args))
		(error ,msg ,@args)
	))

(defmacro exo-cfg-error (msg cfg-path &rest args)
	`(if *is-interactive*
		(error '<exo-cfg-error> :message (fmt ,msg ,@args) :cfg-path ,cfg-path)
		(error ,msg ,@args)
	))

;;;;;;;;;;;;;;;;;;;;;;
;;;   exo module   ;;;
;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:exo-mod
	(:use #:cl
		#:exo-utils
		#:exo-fs
		#:exo-plist
		#:exo-error)
	(:shadowing-import-from #:exo-utils
		#:write-to-string
		#:read-from-string
		#:string-downcase
		#:string-upcase)
	(:export
		#:%mod-file-name%
		#:%hashes-file-name%
		#:%sig-file-name%
		#:is-run-prop
		#:get-run-value
		#:module-path
		#:<module>
		#:make-module
		#:.check-dependencies
		#:.check-run-props
		#:.props
		#:.cfg-path
		#:.use
		#:.require
		#:.dependencies
		#:.signature
		#:.id
		#:.version
		#:.run))
(in-package #:exo-mod)

(defvar %mod-file-name% "exo.mod")
(defvar %hashes-file-name% ".hashes")
(defvar %sig-file-name% (string+ %mod-file-name% ".sig"))
(defvar %package-function-delimiter% "@")

;;; parse exo.mod ;;;

(declaim (inline ensure-even))
(defun ensure-even (lst)
	(unless (zerop (mod (length lst) 2))
		(error "Odd number of values in property list. Expected (:key \"<value>\" ...) in: ~s" lst))
	lst)

(declaim (inline ensure-key))
(defun ensure-key (value &optional obj)
	(unless (keywordp value)
		(error "Value ~s is not a keyword in: ~s" value obj))
	; todo: is need to validate a key name?
	value)

(defun id-ver-list-p (value)
	(when (consp value)
		(if (and
				(eq (length value) 2)
				(keywordp (first value))
				(stringp (second value)))
			t
			(exo-error "Invalid module path ~s . Expected (:mod-id \"mod-ver\")" value)
		)))

(declaim (inline is-run-prop))
(defun is-run-prop (prop)
	(or
		(eq prop :run)
		(string-starts-with (string-downcase prop) "run-")
		(eq prop :test)
		(string-starts-with (string-downcase prop) "test-")
	))

(defun get-run-prop (prop)
	(if prop
		(if (is-run-prop prop)
			prop
			(exo-error "Invalid run property ~(~s~). Expected :run :run-... or :test :test-..." prop))
		:run
	))

(deftype module-path ()
	`(or string (satisfies id-ver-list-p)))

;;; class module ;;;

(defclass <module> () (
	(cfg :initarg :cfg :reader .cfg)
	(props :initarg :props :reader .props)
	(path :initform nil :reader .path)
	(cfg-path :initform nil :reader .cfg-path)
	(id :initform nil :reader .id)
	(version :initform nil :reader .version)
	(run :initform nil :reader .run)
))

(defmethod .use ((obj <module>))
	(plist/get (.props obj) :module :use))

(defmethod .require ((obj <module>))
	(plist/get (.props obj) :module :require))

(defmethod .dependencies ((obj <module>))
	(plist/get (.props obj) :module :dependencies))

(defmethod .signature ((obj <module>))
	"Return key and urls list"
	(lets (prop (plist/get (.props obj) :module :signature))
		(when prop
			(values
				(plist/get prop :key)
				(plist/get prop :urls))
		)))

(defmethod .find-source ((obj <module>) src-id)
	(check-type src-id keyword)
	(let-when (sources (plist/get (.props obj) :module :sources)) sources
		(iterate-by sources 2
			(lambda (src-type values)
				(iterate-by values 4
					(lambda (id owner-repo btc btc-val)
						(when (eq src-id id)
							(return-from .find-source
								(values src-type owner-repo btc btc-val))))
				)))))

(defmacro deps-error (msg &rest args)
	`(exo-error (string+ "Error read :dependencies property. " ,msg) ,@args))

(defmethod .check-dependencies ((obj <module>) &optional check-fn)
	(unless (evenp (length (.dependencies obj)))
		(deps-error "Odd number of values."))
	(lets (mod-id-list ())
		(iterate-by (.dependencies obj) 2 (lambda (mod-id mod-ver)
			(when (member mod-id mod-id-list)
				(deps-error "Duplicated mod-id ~(~s~)" mod-id))
			(push mod-id mod-id-list)
			(unless (keywordp mod-id)
				(deps-error "Module id ~s is not a keyword" mod-id))
			(unless (stringp mod-ver)
				(deps-error "Module version value ~s is not a string" mod-ver))
			(when check-fn
				(funcall check-fn mod-id mod-ver))
		))))

(defmethod .check-run-props ((obj <module>) &optional (mod-path (.path obj)))
	(check-type mod-path string)
	(lets (keys () values ())
		(iterate-by (plist/get (.props obj) :module) 2 (lambda (key value)
			(when (is-run-prop key)
				(when (find key keys :test 'equal)
					(exo-error "Key ~(~s~) is duplicated" key))
				(when (find value values :test 'equal)
					(exo-error "Value ~(~s~) is duplicated for key ~(~s~)" value key))
				(lets (value
							(string-downcase
								(typecase value
									(keyword value)
									(cons (first value))
									(otherwise (exo-error "Invalid value ~(~s~) for property ~(~s~). Only :keyword or (:keyword (args)) is allowed."
										value key))
								)))
					(push key keys)
					(push value values)
					(when (string-starts-with value %package-function-delimiter%)
						(setf value (string+ (string-downcase (plist/get (.props obj) :module :id)) value)))
					(let-unless (file-path (string+ mod-path "src/"
							(first (uiop:split-string value :separator %package-function-delimiter%)) ".lisp")) (probe-file file-path)
						(exo-error "Invalid property (~(~s~) ~s). A package file ~s does not exist" key value file-path)
					)))))))

(defmethod print-object ((obj <module>) out)
	(print-unreadable-object (obj out :type t)
		;(format out "~%  path: ~s" (.path obj))
		;(format out "~%  cfg: ~s" (.cfg-path obj))
		(format out "~%  :id ~(~s~)" (.id obj))
		(format out "~%  :version ~s" (.version obj))
		(iterate-by (plist/get (.props obj) :module) 2 (lambda (key value)
			(when (is-run-prop key)
				(format out "~%  :~(~a~) ~(~s~)" key value))))
		(format out "~%  :use ~(~s~)" (.use obj))
		(when (.dependencies obj)
			(format out "~%  :dependencies ~(~s~)" (.dependencies obj)))
		(format out "~%  :signature ~a~%" (if (.signature obj) "yes" "no"))
	))

(defun module-props-p (props)
	(if (consp props)
		(plist/check props '(
			(:id keywordp)
			(:version stringp)
			;(:run stringp)
			;(:test stringp)
			(:use consp)
			(:require consp)
			(:dependencies consp)
			(:signature consp)
		) #'is-run-prop)
		(exo-error "Invalid property :module. Value type is not consp")
	))

(defmethod initialize-instance :after ((obj <module>) &key cfg props check-run-props)
	(handler-case
		(progn
			(unless (equal (.name cfg) %mod-file-name%)
				(exo-error "Invalid module config name ~s. Expected name is ~s" (.name cfg) %mod-file-name%))
			(plist/ensure props :exo-version :module)
			(plist/ensure (plist/get props :module) :id :version)
			(plist/check props '(
				(:exo-version floatp)
				(:name stringp)
				(:description stringp)
				(:module module-props-p)
				(:properties consp)
				(:about consp)))
			(when check-run-props
				(.check-run-props obj (.parent cfg)))
			(.check-dependencies obj)
			(plist/ensure-unique-keys props)
			(plist/ensure-unique-keys (plist/get props :module)))
		(<exo-error> (c)
			(exo-cfg-error (.message c) (.path cfg)))
		(error (c)
			(exo-cfg-error (fmt "~a" c) (.path cfg)))
	))

(defun make-module (cfg-path &optional (check-run-props t))
	(check-type cfg-path (or string))
	(check-type check-run-props boolean)
	(lets (
			cfg (make-file-link cfg-path :ensure-file)
			props (plist/read cfg-path)
			mod (make-instance '<module>
				:cfg cfg
				:props props
				:check-run-props check-run-props))
		(setf
			(slot-value mod 'path) (.parent cfg)
			(slot-value mod 'cfg-path) (.path cfg)
			(slot-value mod 'id) (plist/get props :module :id)
			(slot-value mod 'version) (plist/get props :module :version)
			(slot-value mod 'run) (plist/get props :module :run))
		mod
	))

;(declaim (inline get-run-value))
(defun get-run-value (mod run-prop)
	"Return (\"<package>\") ~
		or (\"<package>\" \"<function>\") ~
		or (\"<package>\" \"<function>\" (args))"
	(check-type run-prop (or null keyword cons))
	(lets (
			run-value
				(string-downcase
					(typecase run-prop
						((or null keyword)
							(plist/get (.props mod) :module (get-run-prop run-prop)))
						(cons (first run-prop))
						(otherwise
							(exo-error "Wrong type of run property: ~s. Null, keyword or cons are allowed" run-prop))))
			run-value
				(if run-value
					(if (string-starts-with run-value %package-function-delimiter%)
						(string+ (string-downcase (.id mod)) run-value)
						run-value)
					(exo-error "Could't run module. Run parameter ~(~s~) is not defined" run-prop))
			result
				(ensure-length
					(uiop:split-string run-value :separator %package-function-delimiter%) 1 2
					(fmt "Invalid run value ~s. Expected \"<package>\" or \"<package>@<function>\"" run-value)
					:return :sequence))
		(if (and (consp run-prop) (> (length run-prop) 1))
			(if (eq (length result) 1)
				(append result (list nil (rest run-prop)))
				(append result (list (rest run-prop))))
			result)
	))

;;;;;;;;;;;;;;;;;;;;
;;;   exo repo   ;;;
;;;;;;;;;;;;;;;;;;;;

(defpackage #:exo-repo
	(:use #:cl
		#:exo-utils
		#:exo-fs
		#:exo-plist
		#:exo-error
		#:exo-mod)
	(:shadowing-import-from #:exo-utils
		#:write-to-string
		#:read-from-string
		#:string-downcase
		#:string-upcase)
	(:export
		#:%repo-file-name%
		#:.secure
		#:.signatures
		#:check-repository-props
		#:ensure-repo-set
		#:make-repo-tmp-dir
		#:repo-path
		#:repo-check
		#:repo-does-not-set!
		#:cancel-repo
		#:do-search
	))
(in-package #:exo-repo)

(defparameter *repository* ())

(defvar %repo-file-name% "exo.repo")
(defvar %repo-file-content% (fmt
"(
	:exo-version ~d
	:repository (
		:secure nil
	)
)" (exo:version) ))

(defvar %repo-tmp-dir-name% ".tmp")
(defparameter *repo-tmp-path* ())

(defun repo-check (path)
	"Check directory of the repository. Return dir or throw an error"
	(check-type path (or string))
	(lets (dir (make-file-link path :ensure-dir))
		(unless (probe-file (string+ (.path dir) %repo-file-name%))
			(exo-error "Directory ~s is not a repository: file ~s does not exist" path %repo-file-name%))
		dir
	))

;;; class repository ;;;

(defclass <repository> () (
	(cfg :initarg :cfg :reader .cfg)
	(props :initarg :props :reader .props)
	(cfg-path :initform nil :reader .cfg-path)
	(path :initform nil :reader .path)
	(tmp-path :initform nil :reader .tmp-path)
	(secure :initform nil :reader .secure)
	(signatures :initform nil :reader .signatures)
))

(defmethod .sources ((obj <repository>))
	(plist/get (.props obj) :repository :sources))

(defmethod print-object ((obj <repository>) out)
	(print-unreadable-object (obj out :type t)
		(format out "~%  path: ~s" (.path obj))
		(format out "~%  cfg: ~(~s~)" (.cfg-path obj))
		(format out "~%  secure: ~(~s~)" (.secure obj))
		(format out "~%  signatures: ~a~%" (if (.signatures obj) "yes" "no"))
	))

(defmacro sources-error (msg &rest args)
	`(exo-error (fmt "Error read :sources property. ~a" ,msg) ,@args))

(declaim (inline sources-props-p))
(defun sources-props-p (props)
	(if (consp props)
		(handler-case
			(progn
				(unless (zerop (mod (length props) 4))
					(error "Number of elements (~d) must be a multiple of ~d" (length props) 4))
				(iterate-by props 4 (lambda (source repo-path btc btc-val)
					(check-type source (member :codeberg :github))
					(check-type repo-path string)
					(check-type btc (member :branch))
					(check-type btc-val string)
				))t)
			(error (c)
				(sources-error (fmt "~a" c))))
		(exo-error "Invalid property :sources. Value type is not cons")
	))

(defun repository-props-p (props)
	(if (consp props)
		(plist/check props '(
			(:secure booleanp)
			(:sources sources-props-p)
			(:signatures consp)
		))
		(exo-error "Invalid property :repository. Value type is not cons")
	))

(defun check-repository-props (props)
	(plist/check props '(
		(:exo-version floatp)
		(:name stringp)
		(:description stringp)
		(:repository repository-props-p)
		(:about consp))))

(defmethod initialize-instance :after ((obj <repository>) &key cfg props)
	(handler-case
		(progn
			(unless (equal (.name cfg) %repo-file-name%)
				(exo-error "Invalid repository config name. Expected name is ~s" %repo-file-name%))
			(plist/ensure props :exo-version)
			(check-repository-props props)
			(plist/ensure-unique-keys props)
			(plist/ensure-unique-keys (plist/get props :repository)))
		(<exo-error> (c)
			(exo-cfg-error (.message c) (.path cfg)))
		(error (c)
			(exo-cfg-error (fmt "~a" c) (.path cfg)))
	))

(defun make-repository (path &optional cfg-path)
	(check-type cfg-path (or null string))
	(lets (
			dir (repo-check path)
			cfg (make-file-link
					(if cfg-path cfg-path (string+ (.path dir) %repo-file-name%))
					:ensure-file)
			props (plist/read (.path cfg))
			repo (make-instance '<repository> :cfg cfg :props props))
		(setf
			(slot-value repo 'cfg-path) (.path cfg)
			(slot-value repo 'props) props
			(slot-value repo 'path) (.path dir)
			(slot-value repo 'tmp-path) (string+ (.path dir) ".tmp/" )
			(slot-value repo 'secure) (plist/get props :repository :secure)
			(slot-value repo 'signatures) (plist/get props :repository :signatures))
		(ensure-directories-exist (.tmp-path repo))
		repo
	))

(declaim (inline repo-path))
(defun repo-path ()
	(when *repository*
		(.path *repository*)))

(declaim (inline repo-does-not-set!))
(defun repo-does-not-set! ()
	(exo-error "Repository does not set"))

(declaim (inline ensure-repo-set))
(defun ensure-repo-set ()
	(unless *repository*
		(repo-does-not-set!)))

(declaim (inline repo-tmp-path))
(defun repo-tmp-path ()
	(locally (declare (notinline ensure-repo-set))
		(ensure-repo-set))
	(.tmp-path *repository*))

(defun make-repo-tmp-path ()
	(string+ (repo-tmp-path) (make-random-string 10)))

;(declaim (inline make-repo-tmp-dir))
(defun make-repo-tmp-dir ()
	(ensure-directories-exist
		(string+ (make-repo-tmp-path) "/")))

;(declaim (inline make-repo-tmp-file))
(defun make-repo-tmp-file (&optional content)
	"Return (file-path stream)"
	(lets (
			file-path (make-repo-tmp-path)
			stream (open file-path :direction :output))
		(when content
			(write-string content stream)
			(close stream))
		(values file-path
			(unless content stream))
	))

(defun create-repo (path)
	(pfmtl "::: Create repository ~s :::" path)
	(ensure-directories-exist
		(if (string-ends-with path "/")
			path
			(string+ path "/")))
	(let-if (
			dir (make-file-link path :ensure-dir)
			repo-cfg-path (string+ (.path dir) %repo-file-name%)) (probe-file repo-cfg-path)
		(exo-error "Repository already created in ~s" path)
		(let-if (files (directory-content (.path dir))) files
			(exo-error "Directory ~s is not empty" path)
			(progn
				(file-write-content repo-cfg-path %repo-file-content%)
				(prog1
					(setf *repository* (make-repository path repo-cfg-path))
					(pfmtl "Done")))
		)))

(defun exo:repo (&optional path option)
	(check-type path (or null string))
	(check-type option (or null keyword string))
	(handler-case
		(if path
			(typecase option
				(keyword
					(case option
						(:create (create-repo path))
						(otherwise (exo-error "Unknown option ~(~a~)" option))))
				((or null string)
					(pfmtl "::: Set repository ~s :::" path)
					(setf *repository* (make-repository path option))
					(directory-delete-content (repo-tmp-path))
					(pfmtl "Done")))
			*repository*)
		(<exo-error> (c)
			(.print c))
	))

(defun cancel-repo ()
	(setf *repository* nil))

(defun do-search (mod-id &optional mod-ver (path (repo-path)))
	(ensure-repo-set)
	(let-when (
			result ()
			id-dir-pname
				(probe-file (string+ path (string-downcase mod-id)))) id-dir-pname
		(if mod-ver
			(let-when (ver-dir-pname (probe-file
					(string+ (directory-namestring id-dir-pname) mod-ver))) ver-dir-pname
				(push (make-module (string+ (directory-namestring ver-dir-pname) %mod-file-name%)) result))
			(dolist (pname (directory-content id-dir-pname :dir))
				(push (make-module (string+ (directory-namestring pname) %mod-file-name%)) result)))
		result
	))

;;;;;;;;;;;;;;;;;;;;;;;
;;;   exo service   ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:exo-service
	(:use #:cl
		#:exo-utils
		#:exo-fs
		#:exo-fetch
		#:exo-plist
		#:exo-error)
	(:shadowing-import-from #:exo-utils
		#:read-from-string
		#:write-to-string
		#:string-downcase
		#:string-upcase)
	(:import-from #:exo-mod
		#:%mod-file-name%
		#:%sig-file-name%
		#:.id)
	(:import-from #:exo-repo
		#:%repo-file-name%)
	(:export
		#:make-service-request
		#:.service-id
		#:.owner-repo
		#:.btc
		#:.btc-val
		#:.mod-id
		#:.mod-ver
		#:make-service
		#:.repo-tree
		#:.cfg-type
		#:.search-module
		#:.fetch-file
		#:.fetch-directory-content
		#:.fetch-repo-content
		#:.fetch-cfg
		#:get-git-hash))
(in-package #:exo-service)

; https://stackoverflow.com/questions/7225313/how-does-git-compute-file-hashes/7225329#7225329
(defun get-git-hash (file-path)
	(check-type file-path string)
	(lets (out (make-string-output-stream))
		(uiop:run-program
			(concatenate 'string "git hash-object " file-path)
			:output out)
		(close out)
		(subseq (get-output-stream-string out) 0 40)
	))

;;; tokens ;;;

(defparameter *github-token* ())
(defparameter *codeberg-token* ())

(defun exo:token (service &optional value)
	(check-type service (member nil :github :codeberg))
	(check-type value (or null string))
	(case service
		(:github
			(if value
				(setf *github-token* value)
				*github-token*))
		(:codeberg
			(if value
				(setf *codeberg-token* value)
				*codeberg-token*))
	))

(defclass <git-file-validator> (<file-validator>)())

(defmethod .validate ((obj <git-file-validator>) file-path file-hash error-msg)
	(lets (calc-file-hash (get-git-hash file-path))
		(unless (equal file-hash calc-file-hash)
			(error error-msg file-path file-hash calc-file-hash))
	))

(defvar %git-file-validator% (make-instance '<git-file-validator>))

(defun fetch-git-directory-content (url to-dir &optional headers)
	(check-type url string)
	(check-type to-dir string)
	(check-type headers (or null list))
	(pfmtl "Fetch git directory content: ~s" url)
	(ensure-directories-exist to-dir)
	(lets (tree-props (fetch-json-data url headers))
		(dolist (prop (plist/get tree-props :tree))
			(if (equal (plist/get prop :type) "blob")
				(fetch-file to-dir
					(plist/get prop :path)
					(plist/get prop :url)
					headers
					(plist/get prop :sha)
					%git-file-validator%)
				(fetch-git-directory-content
					(plist/get prop :url)
					(string+ to-dir (plist/get prop :path) "/")
					headers))
		)))

;;; service request ;;;

(defun make-service (service-id tmp-dir)
	(lets (dir (make-file-link tmp-dir :ensure-dir))
		(case service-id
			(:codeberg
				(make-instance '<codeberg-service> :tmp-dir (.path dir)))
			(:github
				(make-instance '<github-service> :tmp-dir (.path dir)))
			(otherwise (exo-error "Unknown service id: ~(~s~)" service-id))
		)))

(defclass <service-request> () (
	(service-id :initarg :service-id :reader .service-id)
	(owner-repo :initarg :owner-repo :reader .owner-repo)
	(btc :initarg :btc :reader .btc)
	(btc-val :initarg :btc-val :reader .btc-val)
	(mod-id :initarg :mod-id :accessor .mod-id)
	(mod-ver :initarg :mod-ver :accessor .mod-ver)
))

(defmethod print-object ((obj <service-request>) out)
	(print-unreadable-object (obj out :type t)
		(format out "~%  :service-id ~(~s~)" (.service-id obj))
		(format out "~%  :owner-repo ~s" (.owner-repo obj))
		(format out "~%  :btc ~(~s~)" (.btc obj))
		(format out "~%  :btc-val ~s" (.btc-val obj))
		(format out "~%  :mod-id ~(~s~)" (.mod-id obj))
		(format out "~%  :mod-ver ~s" (.mod-ver obj))
	))

(defun make-service-request (service-id owner-repo btc btc-val mod-id mod-ver)
	(make-instance '<service-request>
		:service-id service-id
		:owner-repo owner-repo
		:btc btc :btc-val btc-val
		:mod-id mod-id :mod-ver mod-ver))

;;; class service ;;;

(defclass <service> () (
	(tmp-dir :initarg :tmp-dir :reader .tmp-dir)
))

(defgeneric .repo-tree-url (service owner-repo btc btc-val))
(defgeneric .repo-tree (service owner-repo btc btc-val))
(defgeneric .cfg-type (service tree-props))
(defgeneric .search-module (service request &optional repo-tree))
(defgeneric .fetch-file (service to-dir file-name url &optional sha))
(defgeneric .fetch-data (service url))
(defgeneric .fetch-json-data (service url))
(defgeneric .fetch-directory-content (service url &optional to-dir))
(defgeneric .fetch-repo-content (service owner-repo btc btc-val &optional to-dir))
(defgeneric .fetch-cfg (service owner-repo btc btc-val))

;;; class codeberg service ;;;

(defclass <codeberg-service> (<service>) (
	(id :initform :codeberg :reader .id)
	(base-url :initform "https://codeberg.org" :reader .base-url)
	(headers :initform (list
		(when *codeberg-token*
			(cons "Authorization" (string+ "token " *codeberg-token*))
		)) :reader .headers)
))

(defmethod .repo-tree-url ((obj <codeberg-service>) owner-repo btc btc-val)
	(lets (repo-url (string+ (.base-url obj) "/api/v1/repos/" owner-repo))
		(case btc
			(:branch
				(string+ repo-url "/contents?ref=" btc-val))
			(:tag (error "Not implemented yet"))
			(:commit (error "Not implemented yet"))
		)))

(defmethod .repo-tree ((obj <codeberg-service>) owner-repo btc btc-val)
	(fetch-json-data (.repo-tree-url obj owner-repo btc btc-val) (.headers obj)))

(defmethod .cfg-type ((obj <codeberg-service>) tree-props)
	"Returns (type url sha)"
	(dolist (item tree-props)
		(lets (
				path (plist/get item :name)
				type (plist/get item :type))
			(when (and (equal path %mod-file-name%) (equal type "file"))
				(return-from .cfg-type
					(values :mod (plist/get item :download_url) (plist/get item :sha))))
			(when (and (equal path %repo-file-name%) (equal type "file"))
				(return-from .cfg-type
					(values :repo (plist/get item :download_url) (plist/get item :sha))))
		))
	(exo-error "Source does not contain module ~s or repository ~s file"
		%mod-file-name% %repo-file-name%))

(defmethod .search-module ((obj <codeberg-service>) request &optional repo-tree)
	(pfmtl "Search module: (~(~s~) ~s ~(~s~) ~s ~(~s~) ~s)"
		(.service-id request) (.owner-repo request)
		(.btc request) (.btc-val request)
		(.mod-id request) (.mod-ver request))
	(let-when (
			repo-tree
				(if repo-tree repo-tree
					(.repo-tree obj (.owner-repo request) (.btc request) (.btc-val request)))
			mod-dir-prop (plist/find repo-tree :name (string-downcase (.mod-id request)))) mod-dir-prop
		(lets (
				mod-dir-tree (.fetch-json-data obj (plist/get mod-dir-prop :url))
				ver-dir-prop (plist/find mod-dir-tree :name (.mod-ver request)))
			(unless ver-dir-prop
				(exo-error "Version directory ~(~s~) does not exist for module ~(~s~)"
					(.mod-ver request) (.mod-id request)))
			(lets (
					ver-dir-tree (.fetch-json-data obj (plist/get ver-dir-prop :url))
					mod-cfg-prop (plist/find ver-dir-tree :name %mod-file-name%)
					mod-sig-prop (plist/find ver-dir-tree :name %sig-file-name%))
				(unless mod-cfg-prop
					(exo-error "Module config ~s does not exist" %mod-file-name%))
				(values
					(plist/find ver-dir-tree :name "src")
					(plist/get mod-cfg-prop :download_url) (plist/get mod-cfg-prop :sha)
					(plist/get mod-sig-prop :download_url) (plist/get mod-sig-prop :sha))
			))))

(defmethod .fetch-file ((obj <codeberg-service>) to-dir file-name url &optional sha)
	(fetch-file to-dir file-name url (.headers obj) sha %git-file-validator%))

(defmethod .fetch-data ((obj <codeberg-service>) url)
	(fetch-data url (.headers obj)))

(defmethod .fetch-json-data ((obj <codeberg-service>) url)
	(fetch-json-data url (.headers obj)))

(defmethod .fetch-directory-content ((obj <codeberg-service>) url &optional (to-dir (.tmp-dir obj)))
	(check-type url string)
	(check-type to-dir string)
	(pfmtl "Fetch directory content: ~s" url)
	(ensure-directories-exist to-dir)
	(lets (
			headers (.headers obj)
			tree-props (fetch-json-data url headers))
		(dolist (prop tree-props)
			(if (equal (plist/get prop :type) "file")
				(fetch-file to-dir
					(plist/get prop :name)
					(plist/get prop :download_url)
					headers
					(plist/get prop :sha)
					%git-file-validator%)
				(.fetch-directory-content obj
					(plist/get prop :url)
					(string+ to-dir (plist/get prop :name) "/")))
		)))

(defmethod .fetch-repo-content ((obj <codeberg-service>) owner-repo btc btc-val &optional (to-dir (.tmp-dir obj)))
	(let-bind (type props) (.fetch-cfg obj owner-repo btc btc-val)
		(declare (ignore props))
		(pfmtl "Found ~a configuration file"
			(case type
				(:mod "module")
				(:repo "repository")
			))
		(.fetch-directory-content obj (.repo-tree-url obj owner-repo btc btc-val) to-dir)
	))

(defmethod .fetch-cfg ((obj <codeberg-service>) owner-repo btc btc-val)
	(let-bind (type url sha)
			(.cfg-type obj (.repo-tree obj owner-repo btc btc-val))
		(declare (ignore sha))
		(values type (read-from-string (.fetch-data obj url)))
	))

;;; github service ;;;

(defclass <github-service> (<service>) (
	(id :initform :github :reader .id)
	(base-url :initform "https://api.github.com" :reader .base-url)
	(headers :initform (list
		(cons "Accept" "application/vnd.github.raw+json")
		(when *github-token*
			(cons "Authorization" (string+ "Bearer " *github-token*))
		)) :reader .headers)
))

(defmethod .repo-tree-url ((obj <github-service>) owner-repo btc btc-val)
	(lets (repo-url (string+ (.base-url obj) "/repos/" owner-repo))
		(case btc
			(:branch
				(string+ repo-url "/branches/" btc-val))
			(:tag (error "Not implemented yet"))
			(:commit (error "Not implemented yet"))
		)))

(defmethod .repo-tree ((obj <github-service>) owner-repo btc btc-val)
	(lets (headers (.headers obj))
		(fetch-json-data
			(plist/get (fetch-json-data (.repo-tree-url obj owner-repo btc btc-val) headers)
				:commit :commit :tree :url)
			headers)
	))

(defmethod .cfg-type ((obj <github-service>) tree-props)
	"Returns (type url sha)"
	(dolist (item tree-props)
		(lets (
				path (plist/get item :path)
				type (plist/get item :type))
			(when (and (equal path %mod-file-name%) (equal type "blob"))
				(return-from .cfg-type
					(values :mod (plist/get item :url) (plist/get item :sha))))
			(when (and (equal path %repo-file-name%) (equal type "blob"))
				(return-from .cfg-type
					(values :repo (plist/get item :url) (plist/get item :sha))))
		))
	(exo-error "Source does not contains module or repository file"))

(defmethod .search-module ((obj <github-service>) request &optional repo-tree)
	(pfmtl "Search module: (~(~s~) ~s ~(~s~) ~s ~(~s~) ~s)"
		(.service-id request) (.owner-repo request)
		(.btc request) (.btc-val request)
		(.mod-id request) (.mod-ver request))
	(let-when (
			repo-tree
				(if repo-tree repo-tree
					(plist/get (.repo-tree obj (.owner-repo request) (.btc request) (.btc-val request)) :tree))
			mod-dir-prop (plist/find repo-tree :path (string-downcase (.mod-id request)))) mod-dir-prop
		(lets (
				mod-dir-tree (plist/get (.fetch-json-data obj (plist/get mod-dir-prop :url)) :tree)
				ver-dir-prop (plist/find mod-dir-tree :path (.mod-ver request)))
			(unless ver-dir-prop
				(exo-error "Version directory ~(~s~) does not exist for module ~(~s~)"
					(.mod-ver request) (.mod-id request)))
			(lets (
					ver-dir-tree (plist/get (.fetch-json-data obj (plist/get ver-dir-prop :url)) :tree)
					mod-cfg-prop (plist/find ver-dir-tree :path %mod-file-name%)
					mod-sig-prop (plist/find ver-dir-tree :path %sig-file-name%))
				(unless mod-cfg-prop
					(exo-error "Module config ~s does not exist" %mod-file-name%))
				(values
					(plist/find ver-dir-tree :path "src")
					(plist/get mod-cfg-prop :url) (plist/get mod-cfg-prop :sha)
					(plist/get mod-sig-prop :url) (plist/get mod-sig-prop :sha))
			))))

(defmethod .fetch-file ((obj <github-service>) to-dir file-name url &optional sha)
	(fetch-file to-dir file-name url (.headers obj) sha %git-file-validator%))

(defmethod .fetch-data ((obj <github-service>) url)
	(fetch-data url (.headers obj)))

(defmethod .fetch-json-data ((obj <github-service>) url)
	(fetch-json-data url (.headers obj)))

(defmethod .fetch-directory-content ((obj <github-service>) url &optional (to-dir (.tmp-dir obj)))
	(fetch-git-directory-content url to-dir (.headers obj)))

(defmethod .fetch-repo-content ((obj <github-service>) owner-repo btc btc-val &optional (to-dir (.tmp-dir obj)))
	(let-bind (type props) (.fetch-cfg obj owner-repo btc btc-val)
		(declare (ignore props))
		(pfmtl "Found ~a configuration file"
			(case type
				(:mod "module")
				(:repo "repository")
			))
		(.fetch-directory-content obj (.repo-tree-url obj owner-repo btc btc-val) to-dir)
	))

(defmethod .fetch-cfg ((obj <github-service>) owner-repo btc btc-val)
	(let-bind (type url sha)
			(.cfg-type obj (plist/get (.repo-tree obj owner-repo btc btc-val) :tree))
		(declare (ignore sha))
		(values type (read-from-string (.fetch-data obj url)))
	))

;;;;;;;;;;;;;;;;;;;;
;;;   exo sign   ;;;
;;;;;;;;;;;;;;;;;;;;

(defpackage #:exo-sign
	(:use #:cl
		#:exo-utils
		#:exo-fs
		#:exo-error
		#:exo-mod)
	(:shadowing-import-from #:exo-utils
		#:read-from-string
		#:write-to-string
		#:string-downcase
		#:string-upcase)
	(:import-from #:exo-fetch
		#:fetch-data)
	(:import-from #:exo-repo
		#:make-repo-tmp-dir
		#:make-repo-tmp-file
		#:make-repo-tmp-path
		#:do-search)
	(:import-from #:exo-service
		#:get-git-hash)
	(:export
		#:do-sign-verify))
(in-package #:exo-sign)

(declaim (inline make-public-key-file))
(defun make-public-key-file (public-key)
	(make-repo-tmp-file
		(fmt "untrusted comment: signify public key~%~a~%" public-key)))

(defun git-hash-object (file-path)
	(lets (output (make-string-output-stream))
		(uiop:run-program (string+ "git hash-object " file-path)
			:output output :error-output *standard-output*)
		(subseq (get-output-stream-string output) 0 40)
	))

(declaim (inline write-hash-data))
(defun write-hash-data (file-path stream trim-length)
	(write-string (git-hash-object file-path) stream)
	(write-char #\space stream)
	(write-string (subseq file-path trim-length) stream))

(defun make-hashes-file (mod)
	(lets (
			pnames (directory-content-deep (.path mod) :file :dir)
			trim-length (length (.path mod))
			hashes-file-path (string+ (make-repo-tmp-dir) %hashes-file-name%))
		(if pnames
			(with-open-file (stream hashes-file-path :direction :output)
				(write-hash-data (namestring (first pnames)) stream trim-length)
				(dolist (pname (rest pnames))
					(write-char #\newline stream)
					(write-hash-data (namestring pname) stream trim-length)
				))
			(exo-error "No files found in module directory"))
		hashes-file-path
	))

(defmacro sign-error (msg mod &rest args)
	`(exo-error (fmt "Error sign module (~(~s~) ~s). ~a"  (.id ,mod) (.version ,mod) ,msg) ,@args))

(defmacro sign-verify-error (msg mod &rest args)
	`(exo-error (fmt "Error verify signature of module (~(~s~) ~s). ~a" (.id ,mod) (.version ,mod) ,msg) ,@args))

(defun signify-sign (mod secure-key-path secure-key-pass sig-file-path msg-file-path)
	(lets (error-output (make-string-output-stream))
		(handler-case
			(uiop:run-program
				(string+ "signify -Se"
					" -s " secure-key-path
					" -x " sig-file-path
					" -m " msg-file-path)
				:input (make-string-input-stream secure-key-pass)
				:error-output error-output)
			(error ()
				(sign-error (get-output-stream-string error-output) mod))
		)))

(defun signify-verify (mod public-key-path sig-file-path msg-file-path)
	(lets (error-output (make-string-output-stream))
		(handler-case
			(uiop:run-program
				(string+ "signify -Ve"
					" -p " public-key-path
					" -x " sig-file-path
					" -m " msg-file-path)
				:error-output error-output)
			(error ()
				(sign-verify-error (get-output-stream-string error-output) mod))
		)))

(defun get-module (mod-path)
	(typecase mod-path
		(<module> mod-path)
		(cons
			(let-if (
					mod-id (first mod-path) mod-ver (second mod-path)
					mod (first (do-search mod-id mod-ver))) mod
				mod
				(exo-error "Module (~(~s~) ~s) does not exist in the repository ~s"
					mod-id mod-ver (.path (exo:repo)))
			))
		(string (make-module mod-path))
	))

(defun verify-public-key (mod secure-key-path secure-key-pass public-key urls)
	"Ensure that public key matches to secure key"
	(pfmt "Test signature for public key: ~s" public-key)
	(lets (sig-file-path (make-repo-tmp-path))
		(signify-sign mod
			secure-key-path secure-key-pass
			sig-file-path (make-repo-tmp-file "test"))
		(signify-verify mod
			(make-public-key-file public-key)
			sig-file-path
			(make-repo-tmp-path)))
	(pfmtl " - OK")
	; fetch keys and compare
	(dolist (url urls)
		(handler-case
			(lets (key-data (fetch-data url))
				(unless (string-starts-with key-data "untrusted comment:")
					(error (fmt "Fetched data is not valid public key data: [~a]" key-data)))
				(unless (search public-key key-data)
					(error "Public key does not match to fetched key"))
				(pfmtl "~s - OK" url))
			(error (c)
				(progn
					(pfmtl "***** Error verify fetched key *****")
					(pfmtl "~s: ~s" url c))
					(pfmtl "***********************************"))
		)))

(defmacro exo:sign (mod-path secure-key-path &optional secure-key-pass resign-without-ask)
	`(do-sign ',mod-path ,secure-key-path ,secure-key-pass ,resign-without-ask))

(defun do-sign (mod-path secure-key-path secure-key-pass &optional resign-without-ask)
	(check-type mod-path module-path "module config path or (<mod-id> \"<mod-ver\")")
	(check-type secure-key-path string)
	(check-type secure-key-pass (or null string))
	(pfmtl "::: Sign module ~(~s~) :::" mod-path)
	(handler-case
		(lets (
				mod (get-module mod-path)
				sig-file-path (string+ (.path mod) %sig-file-name%))
			;(pfmtl "(~(~s~) ~s)" (.id mod) (.version mod))
			(unless secure-key-pass
				(pfmtl "Enter secure key password:~%")
				(setf secure-key-pass (read-without-echo)))
			(let-bind (public-key urls) (.signature mod)
				(if public-key
					(progn
						(verify-public-key mod secure-key-path secure-key-pass public-key urls)
						(when (probe-file sig-file-path)
							(if (or resign-without-ask (yes-or-no-p "Module is already signed and will re-signed. ~%Continue?"))
								(delete-file sig-file-path)
								(sign-error "Operation canceled" mod)))
						(signify-sign mod
							secure-key-path secure-key-pass
							sig-file-path (make-hashes-file mod)))
					(sign-error  (fmt "Public key is not defined in ~s" (.cfg-path mod)) mod)))
			(pfmtl "Signature file created: ~s~%Done" sig-file-path))
		(<exo-error> (c)
			(.print c))
	))

(defun do-sign-verify (mod-path)
	(check-type mod-path (or <module> module-path) "module config path or (<mod-id> \"<mod-ver\")")
	(lets (mod (get-module mod-path))
		(pfmtl "::: Verify module (~(~s~) ~s) signature :::" (.id mod) (.version mod))
		(let-bind (public-key urls) (.signature mod)
			(declare (ignore urls))
			(let-if (
					sig-file-path (string+ (.path mod) %sig-file-name%)
					sig-file-pname (probe-file sig-file-path)
					hashes-file-path (string+
						(if (exo:repo) (make-repo-tmp-dir) (.path mod)) %hashes-file-name%)) public-key
				(if sig-file-pname
					(progn
						(pfmt "Test signature ~s" sig-file-path)
						(signify-verify mod
							(make-public-key-file public-key)
							sig-file-path hashes-file-path)
						(pfmtl " - OK")
						(pfmtl "Checking hashes ...")
						(file-iterate-lines hashes-file-path (lambda (line)
							(pfmt "~s" line)
							(let-if (
									hash (subseq line 0 40)
									file-path (string+ (.path mod) (subseq line 41))
									calc-hash (get-git-hash file-path)) (equal hash calc-hash)
								(pfmtl " - OK")
								(sign-verify-error "Hashes did't match: valid ~s, calculated ~s" mod hash calc-hash))))
						(pfmtl "Done"))
					(sign-verify-error "File ~s does not exist" mod sig-file-path))
				(sign-verify-error "Public key is not defined" mod))
		)))

(defmacro exo:sign-verify (mod-path)
	(handler-case
		`(do-sign-verify ',mod-path)
		(<exo-error> (c)
			(.print c))
	))

;;;;;;;;;;;;;;;;;;;;
;;;   exo core   ;;;
;;;;;;;;;;;;;;;;;;;;

(defpackage :exo-core
	(:use #:cl
		#:exo-utils
		#:exo-fs
		#:exo-plist
		#:exo-error
		#:exo-mod
		#:exo-repo
		#:exo-service
		#:exo-sign)
	(:shadowing-import-from #:exo-utils
		#:write-to-string
		#:read-from-string
		#:string-downcase
		#:string-upcase))
(in-package #:exo-core)

(defmacro in-package! (name)
	`(eval-when (:compile-toplevel :load-toplevel :execute)
		;(pfmtl "in-package! (~s)" ,name)
		(setf *package* (find-package ,name))
	))

(defun exo:clean (&optional (path (repo-path)) option)
	(check-type path (or null string))
	(check-type option (member nil :all))
	(pfmtl "::: Clean repository ~s :::" path)
	(handler-case
		(progn
			(if path
				(repo-check path)
				(repo-does-not-set!))
			(if option
				(if (yes-or-no-p "All files will be deleted from ~s~%Continue?" path)
					(progn
						(pfmt "Done. Deleted ~d dirs/files from the root directory"
							(length (directory-delete-content path :file :hfile :dir :hdir)))
						(when (equal (probe-file path) (probe-file (repo-path)))
							(cancel-repo)
							(pfmtl "Current repository canceled due to its directory was cleaned!")))
					(pfmt "Operation canceled"))
				(if (yes-or-no-p "All directories will be deleted from ~s~%Continue?" path)
					(pfmt "Done. Deleted ~d directories"
						(length (directory-delete-content path :dir)))
					(pfmt "Operation canceled"))
			))
		(<exo-error> (c)
			(.print c))
	))

(defun do-list (&optional (output :compact) (path (repo-path)))
	(lets (
			mod-pnames (directory-content path :dir)
			result ())
		(dolist (mod-pname mod-pnames)
			(lets (
					mod-dir (make-file-link mod-pname)
					ver-pnames (directory-content mod-pname :dir)
					mod-result ())
				(case output
					(:compact
						(push (make-keyword (.name mod-dir)) mod-result)
						(dolist (ver-pname ver-pnames)
							(lets (ver-dir (make-file-link ver-pname))
								(push (.name ver-dir) mod-result)))
						(push (reverse mod-result) result))
					(:modules
						(dolist (ver-pname ver-pnames)
							(lets (ver-dir (make-file-link ver-pname))
								(push (make-module (string+ (.path ver-dir) %mod-file-name%)) result))))
				)))
		(reverse result)
	))

(defun exo:list (&optional (output :compact) (path (repo-path)))
	"List all modules in repository"
	(check-type output (member :compact :modules))
	(check-type path (or null string))
	(pfmtl "::: List repository ~s :::" path)
	(handler-case
		(progn
			(if path
				(repo-check path)
				(repo-does-not-set!))
			(lets (mods (do-list output path))
				(pfmtl "~{~(~s~)~^~%~}" mods)
				(pfmtl "Done")
				mods))
		(<exo-error> (c)
			(.print c))
	))

(defun exo:search (mod-id &optional mod-ver (path (repo-path)))
	"Search module in the repository. If version does't set then list versions will be returned else returned module"
	(check-type mod-id keyword)
	(check-type mod-ver (or null string))
	(check-type path (or null string))
	(pfmtl "::: Search module (~(~s~)~@[ ~s~]) in repository ~s :::" mod-id mod-ver path)
	(handler-case
		(progn
			(if path
				(repo-check path)
				(repo-does-not-set!))
			(lets (mods (do-search mod-id mod-ver path))
				(pfmtl "~{~(~s~)~^~%~}" mods)
				(pfmtl "Done")
				mods))
		(<exo-error> (c)
			(.print c))
	))

(defun check-mod-dir-consistency (mod)
	(pfmt "Check id/version to consistency to directories...")
	(lets (dir-names (last (pathname-directory (.path mod)) 2))
		(unless (string-equal (.id mod) (first dir-names))
			(exo-error "Module id ~(~s~) does not match to directory: ~s"
				(.id mod) (.path mod)))
		(unless (string-equal (.version mod) (second dir-names))
			(exo-error "Module (~(~s~) ~s) does not match to directory: ~s"
				(.id mod) (.version mod) (.path mod)))
		(pfmtl " - OK")
	))

(defun check-mod-dependencies (mod)
	(pfmt "Check dependencies...")
	(.check-dependencies mod
		(lambda (mod-id mod-ver)
			(unless (do-search mod-id mod-ver)
				(exo-error "Dependency (~(~s~) ~s) does not exist in the repository for module (~(~s~) ~s)."
					mod-id mod-ver (.id mod) (.version mod)))
		))
	(pfmtl " - OK"))

(defun check-mod-signature (mod)
	(pfmt "Check signature...")
	(if (.signature mod)
		(progn
			(pfmtl " (signed) ")
			(do-sign-verify mod))
		(pfmt " (not signed) "))
	(pfmtl "- OK"))

(defun do-verify (mod)
	(pfmtl "Verify module (~(~s~) ~s)" (.id mod) (.version mod))
	(check-mod-dir-consistency mod)
	(check-mod-dependencies mod)
	(check-mod-signature mod)
	mod)

(defun exo:verify (&optional (path (repo-path)))
	"Verifies each module in repository:~
		- consistency of directory names to module id/version
		- dependencies
		- signatures"
	(check-type path (or null string))
	(pfmtl "::: Verify repository ~s :::" path)
	(handler-case
		(progn
			(if path
				(repo-check path)
				(repo-does-not-set!))
			(lets (mod-list (do-list :modules path))
				(dolist (mod mod-list)
					(do-verify mod))
				(pfmtl "Done")
			))
		(<exo-error> (c)
			(.print c))
	))

(defun exo:remove (mod-id &optional mod-ver (path (repo-path)))
	(check-type mod-id keyword)
	(check-type mod-ver (or null string))
	(check-type path (or null string))
	(pfmtl "::: Remove module (~(~s~)~@[ ~s~]) :::" mod-id mod-ver)
	(handler-case
		(progn
			(if path
				(repo-check path)
				(repo-does-not-set!))
			(if mod-ver
				(let-if (mod (first (do-search mod-id mod-ver path))) mod
					(if (yes-or-no-p "Module (~(~s~) ~s) will be removed from ~s~%Continue?"
							mod-id mod-ver path)
						(directory-delete (.path mod))
						(pfmt "Operation canceled"))
					(exo-error "Module (~(~s~) ~s) does not exist" mod-id mod-ver))
				(let-if (mods (do-search mod-id nil path)) mods
					(if (yes-or-no-p "Module versions ~s will be removed from ~s~%Continue?"
							(loop for mod in mods collect (.version mod)) path)
						(directory-delete
							(uiop:pathname-parent-directory-pathname (.path (first mods))))
						(pfmt "Operation canceled"))
					(exo-error "Module ~(~s~) does not exist" mod-id)))
			(pfmtl "Done"))
		(<exo-error> (c)
			(.print c))
	))

(defmacro install-error (msg mod-id mod-ver &rest args)
	`(exo-error (fmt "Unable to install module (~(~s~) ~s). ~a" ,mod-id ,mod-ver ,msg) ,@args))

(defun install-local (mod-or-cfg &optional request)
	(check-type mod-or-cfg (or string <module>))
	(pfmtl "::: Install module ~s :::"
		(typecase mod-or-cfg
			(string mod-or-cfg)
			(<module> (.cfg-path mod-or-cfg))))
	(ensure-repo-set)
	(lets (
			mod (typecase mod-or-cfg
					(string (make-module mod-or-cfg))
					(<module> mod-or-cfg))
			src-dir-pname (probe-file (string+ (.path mod) "src"))
			mod-id (.id mod) mod-ver (.version mod)
			to-mod-ver-path (string+ (repo-path) (string-downcase mod-id) "/" mod-ver "/")
			to-mod-cfg-path (string+ to-mod-ver-path %mod-file-name%)
			to-mod-src-path (string+ to-mod-ver-path "src/"))
		(when (do-search mod-id mod-ver)
			(install-error "Module is already installed" mod-id mod-ver))
		(unless src-dir-pname
			(install-error "/src directory does not exist" mod-id mod-ver ))
		(unless (directory-content src-dir-pname :file)
			(install-error "/src directory is empty" mod-id mod-ver))
		(let-when (mod-sign
						(let-when (sign (.signature mod)) sign
							(without-output (do-sign-verify mod))
							sign))
						(.secure (exo:repo))
			(let-if (repo-signs
							(let-if (signs (.signatures (exo:repo))) signs
								signs
								(install-error "Repository ~s is secure but signatures are not defined"
									mod-id mod-ver (.path (exo:repo)))
							)) mod-sign
				(unless (plist/find repo-signs :key mod-sign)
					(install-error "Module signed by unknown signature ~s"
						mod-id mod-ver mod-sign))
				(install-error "Repository ~s is secure but module is not signed"
					mod-id mod-ver (.path (exo:repo)))))
		(ensure-directories-exist to-mod-src-path)
		(directory-copy-content src-dir-pname to-mod-src-path)
		(when (.signature mod)
			(let-if (sig-file-pname (probe-file (string+ (.path mod) %sig-file-name%))) sig-file-pname
				(uiop:copy-file sig-file-pname (string+ to-mod-ver-path %sig-file-name%))
				(install-error "Signature defined but signed file ~s does't exist" mod-id mod-ver  %sig-file-name%)))
		(uiop:copy-file (.cfg-path mod) to-mod-cfg-path)
		(lets (mod (make-module to-mod-cfg-path))
			(pfmtl "Module (~(~s~) ~s) installed to ~s " (.id mod) (.version mod) (.path mod))
			(when request
				(file-write-content (string+ (.path mod) ".origin")
					(fmt "(~(~s~) ~s ~(~s~) ~s ~(~s~) ~s)"
						(.service-id request)
						(.owner-repo request)
						(.btc request)
						(.btc-val request)
						(.mod-id request)
						(.mod-ver request))))
			(pfmtl "Done")
		)))

(defun check-owner-repo (value)
	value) ; todo

;; Queue modules to avoid infinity loading of circular dependencies
(defparameter *install-mods-queue* ())

(defun reset-install-queue ()
	(setf *install-mods-queue* ()))

(defun add-to-install-queue (mod-id mod-ver)
	(lets (mod-vers (plist/get *install-mods-queue* mod-id))
		(if mod-vers
			(push mod-ver mod-vers)
			(plist/set *install-mods-queue* mod-id (list mod-ver))
		)))

(defun find-in-install-queue (mod-id mod-ver)
	(check-type mod-id keyword)
	(check-type mod-ver string)
	(let-when (mod-vers (plist/get *install-mods-queue* mod-id)) mod-vers
		(find mod-ver mod-vers :test #'equal)
	))

(defmacro install-dep-error (msg mod-id mod-ver &rest args)
	`(exo-error (fmt "Error install module dependency (~(~s~) ~s). ~a" ,mod-id ,mod-ver ,msg) ,@args))

(defun install-from-service (service request tmp-dir src-dir-prop
		mod-cfg-url mod-cfg-sha mod-sig-url mod-sig-sha from-type)
	(lets (mod (make-module (.fetch-file service tmp-dir %mod-file-name% mod-cfg-url mod-cfg-sha) nil))
		(unless (eq (.mod-id request) (.id mod))
			(exo-error "Mismach module id ~(~s~), expected ~(~s~)" (.id mod) (.mod-id request)))
		(unless (equal (.mod-ver request) (.version mod))
			(exo-error "Mismach module ~(~s~) version ~s, expected ~s"
				(.mod-id request) (.version mod) (.mod-ver request)))
		(unless src-dir-prop
			(exo-error "Empty module. \"src\" directory does not exist"))
		(let-unless (deps (.dependencies mod)) (null deps)
			(when (eq from-type :mod)
				(exo-error "Standalone remote module (~(~s~) ~s) unable to have dependencies"
					(.mod-id request)  (.mod-ver request)))
			(pfmtl "Module dependencies: ~(~s~)" deps)
			(iterate-by deps 2
				(lambda (mod-id mod-ver)
					(pfmtl "Install dependency: (~(~s~) ~s)" mod-id mod-ver)
					(cond
						((do-search mod-id mod-ver)
							(pfmtl "Module (~(~s~) ~s) is already installed" mod-id mod-ver))
						((find-in-install-queue mod-id mod-ver)
							(pfmtl "Module (~(~s~) ~s) in the queue to install" mod-id mod-ver))
						(t
							(setf
								(.mod-id request) mod-id
								(.mod-ver request) mod-ver)
								;(install-dependency service request)))
								(funcall 'install-dependency service request)) ; avoid style warning
					))))
		(.fetch-directory-content service (plist/get src-dir-prop :url) (string+ tmp-dir "src/"))
		(.check-run-props mod)
		(when (and (.signature mod) mod-sig-url)
			(pfmtl "Fetch signature file because module signed")
			(.fetch-file service tmp-dir %sig-file-name% mod-sig-url mod-sig-sha))
		(install-local mod request)
	))

(defun install-dependency (service request)
	(let-bind (src-dir-prop mod-cfg-url mod-cfg-sha mod-sig-url mod-sig-sha)
			(.search-module service request)
		(if src-dir-prop
			(progn
				(pfmtl "Dependency module found in the same remote repository")
				(install-from-service service request (make-repo-tmp-dir) src-dir-prop
					mod-cfg-url mod-cfg-sha mod-sig-url mod-sig-sha :repo))
			(let-bind (cfg-type cfg-props)
					(.fetch-cfg service (.owner-repo request) (.btc request) (.btc-val request))
				(declare (ignore cfg-type))
				(check-repository-props cfg-props)
				(pfmtl "Search dependency module in sources of the remote repository...")
				(let-if (sources (plist/get cfg-props :repository :sources)) sources
					(progn
						(pfmtl "Sources: ~(~s~)" sources)
						(iterate-by sources 4 (lambda (source owner-path btc btc-val)
							(lets (
									service (make-service source (make-repo-tmp-dir))
									request (make-service-request source owner-path btc btc-val
										(.mod-id request) (.mod-ver request)))
								(pfmtl "~a" request)
								(let-bind (src-dir-prop mod-cfg-url mod-cfg-sha mod-sig-url mod-sig-sha)
										(.search-module service request)
									(when src-dir-prop
										(install-from-service service request
											(make-repo-tmp-dir)
											src-dir-prop
											mod-cfg-url mod-cfg-sha
											mod-sig-url mod-sig-sha
											:repo)
										(return-from install-dependency)))
							)))
						(install-dep-error "Module not found in the listed sources"
							(.mod-id request) (.mod-ver request)))
					(install-dep-error "Sources property in remote repository are not defined."
						(.mod-id request) (.mod-ver request))
				)))))

(defun install-from-codeberg (owner-repo btc btc-val mod-id mod-ver tmp-dir)
	(lets (
			service (make-service :codeberg tmp-dir)
			repo-tree (.repo-tree service owner-repo btc btc-val)
			request (make-service-request
				 (.id service) owner-repo btc btc-val mod-id mod-ver))
		; search exo.repo and exo.mod
		(let-bind (type cfg-url cfg-sha)
				(.cfg-type service repo-tree)
			(case type
				(:mod
					(lets (mod-sig-prop (plist/find repo-tree :path %sig-file-name%))
						(install-from-service service request tmp-dir
							(plist/find repo-tree :name "src")
							cfg-url cfg-sha
							(plist/get mod-sig-prop :download_url) (plist/get mod-sig-prop :sha)
							type)))
				(:repo
					(let-bind (src-dir-prop mod-cfg-url mod-cfg-sha mod-sig-url mod-sig-sha)
							(.search-module service request repo-tree)
						(install-from-service service request tmp-dir
							src-dir-prop
							mod-cfg-url mod-cfg-sha
							mod-sig-url mod-sig-sha
							type)
					))))))

(defun install-from-github (owner-repo btc btc-val mod-id mod-ver tmp-dir)
	(lets (
			service (make-service :github tmp-dir)
			repo-tree (plist/get (.repo-tree service owner-repo btc btc-val) :tree)
			request (make-service-request
				 (.id service) owner-repo btc btc-val mod-id mod-ver))
		; search exo.repo or exo.mod
		(let-bind (type cfg-url cfg-sha)
				(.cfg-type service repo-tree)
			(case type
				(:mod
					(lets (mod-sig-prop (plist/find repo-tree :path %sig-file-name%))
						(install-from-service service request tmp-dir
							(plist/find repo-tree :path "src")
							cfg-url cfg-sha
							(plist/get mod-sig-prop :url) (plist/get mod-sig-prop :sha)
							type)))
				(:repo
					(let-bind (src-dir-prop mod-cfg-url mod-cfg-sha mod-sig-url mod-sig-sha)
							(.search-module service request repo-tree)
						(install-from-service service request tmp-dir
							src-dir-prop
							mod-cfg-url mod-cfg-sha
							mod-sig-url mod-sig-sha
							type)
					))))))

(defun install-remote (source owner-repo btc btc-val mod-id mod-ver)
	(check-type owner-repo string)
	(check-type btc (member :branch :tag :commit))
	(check-type btc-val string)
	(check-type mod-id keyword)
	(check-type mod-ver string)
	(check-owner-repo owner-repo)
	(pfmtl "::: Install module (~(~s~) ~s) :::" mod-id mod-ver)
	(pfmtl "Args: (~(~s~) ~s ~(~s~) ~s)"
		source owner-repo btc btc-val)
	(ensure-repo-set)
	(when (do-search mod-id mod-ver)
		(exo-error "Module (~(~s~) ~s) is already installed" mod-id mod-ver))
	(add-to-install-queue mod-id mod-ver)
	(lets (
			tmp-dir (make-repo-tmp-dir)
			mod (case source
					(:github
						(install-from-github owner-repo btc btc-val mod-id mod-ver tmp-dir))
					(:codeberg
						(install-from-codeberg owner-repo btc btc-val mod-id mod-ver tmp-dir))
					))
		mod
	))

(defun exo:install (source &optional owner-repo btc btc-val mod-id mod-ver)
	(check-type source (or string (member :github :codeberg)))
	(handler-case
		(progn
			(reset-install-queue)
			(typecase source
				(string (install-local source))
				(keyword
					(install-remote source owner-repo btc btc-val mod-id mod-ver))
			))
		(<exo-error> (c)
			(.print c))
	))

(defun exo:clone (source owner-repo btc btc-val to-dir)
	(check-type source (member :github :codeberg))
	(check-type btc (member :branch :tag :commit))
	(check-type btc-val string)
	(check-type to-dir string)
	(pfmtl "::: Clone (~(~s~) ~s ~(~s~) ~s) repository to ~s :::"
		source owner-repo btc btc-val to-dir)
	(handler-case
		(let-if (files (directory-content to-dir)) files
			(exo-error "Directory ~s is not empty" to-dir)
			(lets (service (make-service source to-dir))
				(.fetch-repo-content service owner-repo btc btc-val)
				(pfmtl "Done")
			))
		(<exo-error> (c)
			(.print c))
	))

;;; module ;;;

(defparameter *exo-module* ())
(defparameter *exo-modules* ())
(defparameter *exo-bundle* ())

(defvar %intern-pkg-prefix% "EXO+")
(defvar %extern-pkg-prefix% "EXO+")
(defvar %extern-pkg-postfix% "-EXT")

#+exo-debug
(progn
	(defun exo-debug:list-packages ()
		(sort
			(loop for pkg in (list-all-packages)
				when (string-starts-with (package-name pkg) %intern-pkg-prefix%)
				collect pkg)
			(lambda (p1 p2) (string<= (package-name p1) (package-name p2)))
		)))

(defun make-package-keyword (mod-id run-pkg-name)
	(if (string-equal mod-id run-pkg-name)
		(make-keyword (string+ %intern-pkg-prefix% (string mod-id)))
		(make-keyword (string+ %intern-pkg-prefix% (string mod-id) "/" run-pkg-name))
	))

(declaim (inline in-main-module))
(defun in-main-module ()
	(eq *exo-module* (first *exo-modules*)))

(defmacro import-error (msg &rest args)
	`(exo-error (fmt "Error importing package. ~a" ,msg) ,@args))

(defun mod-use-packages (mod package)
	(handler-case
		(dolist (value (.use mod))
			(typecase value
				(keyword
					#+exo-debug
					(pfmtl "Use package ~(~s~) for ~(~s~)" value package)
					(use-package value package))
				(cons
					(lets (
							use-package (first value)
							shadow-symbols (rest value))
						#+exo-debug
						(pfmtl "Use package ~(~s~) for ~(~s~)" use-package package)
						(use-package use-package package)
						#+exo-debug
						(pfmtl "Shadow ~(~s~) for ~(~s~)" shadow-symbols package)
						(dolist (s shadow-symbols)
							(shadow (read-from-string (string+ (package-name use-package) ":" (symbol-name s))) package))
					))
				(otherwise (error "Unexpected type of value ~s. Allowed types: keyword or cons" value))
			))
		(error (c)
			(exo-error "Error read :use property. ~a" c))
	))

(declaim (inline import-symbol))
(defun import-symbol (sym from-package to-package)
	(let-bind (sym2 type) (find-symbol (symbol-name sym) to-package)
		(when sym2
			(if (or (boundp sym2) (fboundp sym2))
				#+!!!
				(exo-error "Unable to import: ~(~s~). Symbol already bounded in ~(~s~). ~
					Maybe need to do shadowing of the necessary symbol before."
					sym2 (package-name to-package))
				(pfmtl "Import warning! Symbol \"~(~a~):~(~a~)\" bounded in ~(~s~) will be replaced."
					(package-name (symbol-package sym2)) (symbol-name sym2) (package-name to-package))
				(unintern sym2 to-package)
			))
		#+exo-debug
		(pfmtl "Import ~(~s~) to ~(~s~)"
			(string+ (symbol-name from-package) ":" (symbol-name sym))
			(package-name to-package))
		; The shadowing in any case to exclude symbol names importing conflict.
		(shadowing-import
			(read-from-string (string+ (symbol-name from-package) ":" (symbol-name sym)))
			to-package)
		#|
		(import
			(read-from-string (string+ (symbol-name from-package) ":" (symbol-name sym)))
			to-package)
		|#
		(when (eq type :external)
			(pfmtl "Re-export ~s in ~s" sym to-package)
			(export sym to-package))
	))

(defun do-import (package-name opt from-package to-package)
	(typecase opt
		(null
			(do-external-symbols (sym (find-package from-package))
				(import-symbol sym from-package to-package)))
		(symbol
			#+exo-debug
			(pfmtl "Import as local nickname: ~(~s~) from ~(~s~) to ~(~s~)" opt from-package (package-name to-package))
			(sb-ext:add-package-local-nickname opt from-package to-package))
		(cons
			(list (dolist (sym opt)
				(import-symbol sym from-package to-package)
			)))
		(otherwise
			(exo-error "Unable to import package ~s. Invalid opt type ~s" package-name opt))
	))

(defun load-bundle (mod &aux (file-path nil))
	(when *exo-bundle*
		(pfmtl "Load bundle: ~(~s~)" (first *exo-bundle*))
		(dolist (pkg (second *exo-bundle*))
			(setf file-path (string+ (.path mod) "src/" (string-downcase pkg) ".lisp"))
			(pfmtl "Load part: ~s" file-path)
			(load file-path))
		(setf *exo-bundle* nil)
	))

(defun import-internal-package (package-name &optional opt)
	(pfmtl "Import internal: ~s => ~(~s~)" package-name (package-name *package*))
	; check the package is not the same package
	(when (string-equal (package-name *package*) (string+ %intern-pkg-prefix% package-name))
		(import-error "Unable to import package to the same package: ~(~s~)" package-name))
	(lets (
			active-package *package*
			active-module (first *exo-modules*)
			package-keyword
				(if (in-main-module)
					(make-package-keyword (.id active-module) package-name)
					(make-keyword (string+ %intern-pkg-prefix%
						(string (.id active-module)) "/" package-name "~" (.version active-module))))
			file-path (string+ (.path active-module) "src/" package-name ".lisp"))
		(pfmtl "Package: ~(~s~)" package-keyword)
		#+!!! ; todo: test and remove if not needed
		(when (string-equal (package-name *package*) package-keyword)
			(import-error "Forbidden to import package to the same package: ~(~s~)" package-name))
		(unless (probe-file file-path)
			(import-error "Package file does not exist: ~s" file-path))
		(unless (find-package package-keyword)
			(make-package package-keyword)
			(import 'exo package-keyword)
			(mod-use-packages active-module package-keyword)
			(in-package! package-keyword)
			(pfmtl "Load ~s" file-path)
			(with-compilation-unit ()
				(load file-path)
				(load-bundle active-module)
				#+exo-debug
				(push (find-package package-keyword) exo-debug:*loaded-packages*)))
		(do-import package-name opt package-keyword active-package)
		(in-package! active-package)
	))

(declaim (inline parse-mod-id))
(defun parse-mod-id (package-name)
	(subseq package-name 0 (position #\/ package-name)))

(declaim (inline trim-mod-id))
(defun trim-mod-id (package-name)
	(let-if (slash-position (position #\/ package-name)) slash-position
		(subseq package-name (1+ slash-position))
		package-name
	))

(defun find-module (mod-id mod-ver)
	(dolist (mod *exo-modules*)
		(when (eq (.id mod) mod-id)
			(return mod)))
	(first (do-search mod-id mod-ver)))

(defun find-mod-ver (mod mod-id)
	(let-if (
			deps (.dependencies mod)
			mod-ver (plist/get deps mod-id)) mod-ver
		(return-from find-mod-ver mod-ver)
		(iterate-by deps 2 (lambda (key value)
			(declare (ignore key))
			(when (consp value)
				(iterate-by value 2 (lambda (key value)
					(when (eq key mod-id)
						(return-from find-mod-ver value))))
			))))
	(exo-error "Module ~(~s~) does not set in dependencies for module ~(~s~)"
		mod-id (.id mod)))

(defun import-external-package (package-name &optional opt)
	(pfmtl "Import external: ~s => ~(~s~)" package-name (package-name *package*))
	(ensure-repo-set)
	(lets (
			active-package *package*
			active-module (first *exo-modules*)
			mod-id (make-keyword (parse-mod-id package-name))
			mod-ver (find-mod-ver active-module mod-id)
			exo-package-name
				(string+ %extern-pkg-prefix% package-name "~" mod-ver)
			package-keyword
				(make-keyword exo-package-name)
			package-keyword-extern
				(make-keyword (string+ exo-package-name %extern-pkg-postfix%)))
		;(pfmtl "Modules: ~s" *exo-modules*)
		(pfmtl "Package: ~(~s~)" package-keyword-extern)
		(let-if (mod (find-module mod-id mod-ver)) mod
			(progn
				(pfmtl "Set active module (~(~s~) ~s)" (.id mod) (.version mod))
				(push mod *exo-modules*)
				(setf active-module mod))
			(import-error "Module (~(~s~) ~s) does not exist in the repository for module ~(~s~)"
				mod-id mod-ver (.id active-module)))
		(let-if (file-path (string+ (.path active-module) "src/" (trim-mod-id package-name) ".lisp")) file-path
			(unless (find-package package-keyword)
				(make-package package-keyword)
				(dolist (keyword (.require active-module))
					(require keyword))
				(import 'exo package-keyword)
				(mod-use-packages active-module package-keyword)
				(in-package! package-keyword)
				(pfmtl "Load ~s" file-path)
				(with-compilation-unit ()
					(load file-path)
					(load-bundle active-module)
					#+exo-debug
					(push (find-package package-keyword) exo-debug:*loaded-packages*)))
			(import-error "Package file does not exist: ~s" file-path))
		(unless (find-package package-keyword-extern)
			(import-error "Package ~s does not export external symbols" package-name))
		(do-import package-name opt package-keyword-extern active-package)
		(in-package! active-package)
		(pop *exo-modules*)
	))

(defun import-runtime-package (package-name &optional opt)
	(pfmtl "Import runtime: ~s => ~(~s~)" package-name (package-name *package*))
	(lets (
			active-package *package*
			package-keyword (make-keyword package-name)
			package (find-package package-keyword))
		(unless package
			(import-error "Package ~(~s~) does not exist" package-keyword))
		(do-import package-name opt package-keyword active-package)
	))

(declaim (inline ensure-symbol-uninterned))
(defun ensure-symbol-uninterned (sym operation)
	(typecase sym
		(symbol
			(when (symbol-package sym)
				(exo-error "Error ~a symbol. ~(~s~) is not uninterned symbol" operation sym)))
		(otherwise (error "Error ~a symbol. ~(~s~) is not a symbol" operation sym))
	))

(defun parse-export (list)
	(unless list
		(exo-error "Export block is empty"))
	(pfmtl "Export from ~(~s~): ~(~s~)" (package-name *package*) list)
	(let (intern extern type)
		(dolist (sym list)
			(if (or (eq sym :intern) (eq sym :extern))
				(setf type sym)
				(progn
					(ensure-symbol-uninterned sym "export")
					(case type
						(:intern (push sym intern))
						(:extern (push sym extern))
						(otherwise (exo-error "Export type (:intern|extern) must be specified first"))))
			))
		(values extern intern)
	))

(defun validate-import-value (value)
	(typecase value
		(cons (dolist (s value)
			(typecase s
				(symbol (ensure-symbol-uninterned s "import"))
				(cons (validate-import-value s))
				(otherwise "Error import symbol. ~(~s~) is not a symbol or cons" s))
		))
		(symbol (ensure-symbol-uninterned value "import"))
	))

(defun parse-import (list)
	(unless list
		(exo-error "Import block is empty"))
	(pfmtl "Import to ~(~s~): ~(~s~)" (package-name *package*) list)
	(let (extern intern runtime type)
		(dolist (sym list)
			(if (or (eq sym :extern) (eq sym :intern) (eq sym :runtime))
				(setf type sym)
				(progn
					(validate-import-value sym)
					(case type
						(:extern (push sym extern))
						(:intern (push sym intern))
						(:runtime (push sym runtime))
						(otherwise (exo-error "Export type (:extern|intern|runtime) must be specified first"))))
			))
		(values (reverse extern) (reverse intern) (reverse runtime))
	))

(defun make-extern-package ()
	(let-if (
			pkg-name (string-upcase+ (package-name *package*) %extern-pkg-postfix%)
			pkg (find-package pkg-name)) pkg
		pkg
		(make-package pkg-name)
	))

(defun export-symbols (list)
	(let-bind (extern intern) (parse-export list)
		(when extern
			(lets (package (make-extern-package))
				(dolist (sym extern)
					(lets (sym2 (find-symbol (symbol-name sym) *package*))
						(if sym2
							(import sym2 package)
							(import
								(intern (symbol-name sym) *package*) package)))
					#+exo-debug
					(pfmtl "Export extern: ~s" sym)
					(export (find-symbol (symbol-name sym) *package*) package)
				)))
		(dolist (sym intern)
			#+exo-debug
			(pfmtl "Export intern: ~s" sym)
			(export (intern (symbol-name sym)))
		)))

(defun import-packages (list fn)
	(dolist (val list)
		(apply fn
			(typecase val
				(cons (list (string-downcase (first val)) (second val)))
				(symbol (list (string-downcase val)))
			))))

(defun short-package-name (package &aux (package-name (package-name package)))
	(lets (slash-pos (position #\/ package-name))
		(setf package-name (subseq package-name (1+
			(if slash-pos slash-pos
				(position #\+ package-name)
			))))
		(string-downcase (subseq package-name 0 (position #\~ package-name)))
	))

(defun set-bundle (type packages &aux (package-name (short-package-name *package*)))
	(pfmtl "Set ~(~s~) ~s: ~(~s~)" type package-name packages)
	(dolist (sym packages)
		(ensure-symbol-uninterned sym "load bundle"))
	(setf *exo-bundle* (list package-name packages))
	; force first load bundle files when :bundle! defined
	(when (eq type :bundle!)
		(load-bundle (first *exo-modules*))
	))

(defmacro exo (def1 &optional def2 def3)
	`(exo:exo ',def1 ',def2 ',def3))

(defun check-def-types (def-types &rest comp)
	(dolist (c comp)
		(unless (eq (length def-types) (length c))
			(exo-error "Inconsistent lengths of def type list: ~(~s~) and compared list: ~(~s~)"
				def-types c))
		(when (equal c def-types) ; compare lists
			(return-from check-def-types)))
		(exo-error "Wrong exo form ~s. Allowed with ~d forms: ~s" def-types (length def-types) comp))

(defun do-def (def)
	(declare (type cons def))
	(case (first def)
		(:import
			(let-bind (extern intern runtime) (parse-import (rest def))
				(import-packages intern #'import-internal-package)
				(import-packages extern #'import-external-package)
				(import-packages runtime #'import-runtime-package)))
		(:export (export-symbols (rest def)))
		((or :bundle :bundle!) (set-bundle (first def) (rest def)))
		(:of-bundle
			(when (eq (length def) 1)
				(exo-error ":of-bundle is empty"))
			(dolist (sym (rest def))
				(ensure-symbol-uninterned sym "load part of bundle"))
			(unless *exo-bundle*
				(exo-error "Forbidden to load ~(~s~) because bundle is not defined" def))
			(unless (find (first *exo-bundle*) (rest def) :test #'string-equal)
				(exo-error "Forbidden to load ~(~s~) to mismatch bundle ~s" def *exo-bundle*)))
	))

(defun check-bundle (def)
	(when (and *exo-bundle* (not (eq (first def) :of-bundle)))
		(exo-error "Forbidden to load package which is not part of bundle: ~s" (first *exo-bundle*))
	))

(defun exo:exo (def1 &optional def2 def3)
	(check-type def1 cons)
	(check-type def2 (or null cons))
	(check-type def3 (or null cons))
	;; TODO: check all defs contains symbols only?
	;(pfmtl "~s ~s~%" def1 def2)
	(cond
		((and def1 def2 def3)
			(lets (bundle-pkgs (rest def3))
				(check-def-types (list (first def1) (first def2) (first def3))
					'(:import :export :bundle) '(:import :export :bundle!))
				(let-bind (extern intern runtime) (parse-import (rest def1))
					(let-when (pkg (members intern bundle-pkgs :or)) pkg
						(exo-error "Forbidden to import package ~s which is part of bundle" pkg))
					(import-packages intern #'import-internal-package)
					(import-packages extern #'import-external-package)
					(import-packages runtime #'import-runtime-package))
				(export-symbols (rest def2))
				(set-bundle (first def3) bundle-pkgs)
			))
		((and def1 def2)
			(check-def-types (list (first def1) (first def2))
				'(:import :export) '(:export :bundle) '(:export :bundle!)
				'(:import :bundle) '(:import :bundle!) '(:export :of-bundle))
			(check-bundle def2)
			(dolist (def (list def1 def2))
				(do-def def)))
		((and def1)
			(check-def-types (list (first def1))
				'(:export) '(:import) '(:bundle) '(:bundle!) '(:of-bundle))
			(check-bundle def1)
			(do-def def1))
	))

(defmacro exo:run (path &optional opt)
	`(do-run ',path ',opt))

(defun do-run (path &optional run-prop)
	(check-type path module-path "module config path or (<mod-id> \"<mod-ver\")")
	(check-type run-prop (or null keyword cons))
	(terpri)
	(pfmtl "::: Run module ~(~s~) :::" path)
	(handler-case
		(lets (
				is-run-standalone (stringp path)
				mod
					(if is-run-standalone
						(make-module path)
						(let-if (
								mod-id (first path) mod-ver (second path)
								mod (first (do-search mod-id mod-ver))) mod
							(do-verify mod)
							(exo-error "Module (~(~s~) ~s) does not exist in the repository ~s"
								mod-id mod-ver (.path (exo:repo)))))
				run-value (get-run-value mod run-prop) ; => (pkg fn (args))
				run-pkg-name (first run-value)
				package-keyword (make-package-keyword (.id mod) run-pkg-name)
				run-fn-name (string+
					(string package-keyword)
					(if (second run-value) (string+ ":" (second run-value)) ":exo-run"))
				active-package *package*
				file-path (string+ (.path mod) "src/" run-pkg-name ".lisp"))
			(pfmtl "~a" mod)
			(lets (mod-sign (.signature mod))
				(when mod-sign
					(without-output (do-sign-verify mod)))
				(unless is-run-standalone
					(let-when (repo-signs (.signatures (exo:repo))) (.secure (exo:repo))
						(unless mod-sign
							(exo-error "Repository ~s is secure but module is not signed" (.path (exo:repo))))
						(unless repo-signs
							(exo-error "Repository ~s is secure but signatures are not defined" (.path (exo:repo))))
						(unless (plist/find repo-signs :key mod-sign)
							(exo-error "Running module (~(~s~) ~s) is forbidden because it signed by unknown signature ~s"
								(.id mod) (.version mod) mod-sign)))))
			(setf
				*exo-module* mod
				*exo-modules* (list mod))
			(if (find-package package-keyword)
				(in-package! package-keyword)
				(progn
					(make-package package-keyword)
					(dolist (keyword (.require mod))
						(require keyword))
					(import 'exo package-keyword)
					(mod-use-packages mod package-keyword)
					(in-package! package-keyword)
					(pfmtl "Load ~s" file-path)
					(with-compilation-unit ()
						(load file-path)
						(load-bundle mod)
						#+exo-debug
						(push (find-package package-keyword) exo-debug:*loaded-packages*))))
			(pfmtl "Call function: ~(~s~)~%" run-fn-name)
			(apply (symbol-function (read-from-string run-fn-name))
				(.path mod)
				(copy-list (.props mod))
				(third run-value))
			(pfmtl "~%Module ~(~s~) execution completed." (.id mod))
			(in-package! active-package)
			mod)
		(<exo-error> (c)
			(.print c))
	))

(defun exo:check-utils ()
	(pfmtl "Check utils..")
	(let (result)
		(let-bind (std-output error-output exit-code)
			(uiop:run-program "curl --version"
				:ignore-error-status t)
			(declare (ignore std-output error-output))
			(push (cons :curl (if (eq exit-code 127) :not-installed :installed)) result))
		(let-bind (std-output error-output exit-code)
			(uiop:run-program "signify"
				:ignore-error-status t)
			(declare (ignore std-output error-output))
			(push (cons :signify (if (eq exit-code 127) :not-installed :installed)) result))
		(let-bind (std-output error-output exit-code)
			(uiop:run-program "git --version"
				:ignore-error-status t)
			(declare (ignore std-output error-output))
			(push (cons :git (if (eq exit-code 127) :not-installed :installed)) result))
		(pfmtl "~{~(~s~)~^~%~}" result)
		(pfmtl "Done")
		result
	))

; Checking installed utils
(dolist (val (without-output (exo:check-utils)))
	(when (eq (cdr val) :not-installed)
		(pfmtl "Warning! ~(~a~) is not installed" (car val))))

; Prevent to use exo functions from outside world/modules.
; We believe that lisp hackers won't reveal this secret!
(dolist
	(p (list
			:exo-error
			:exo-mod
			:exo-repo
			:exo-service
			:exo-sign
			:exo-core))
	(do-external-symbols (s (find-package p))
		(unexport s p)
	))

(defvar %help% "
    ________
   |  KEEP  |         Exo DMT (ver. ~a)
   |  LISP  |         Developed by \"X4J14 Project\"
   | SIMPLE |--__
  /ZZZZZZZZZ/ /O/
  ``````````  ``

Available commands:

(exo:repo \"<repo-path>\" :create) => <repository>
(exo:repo \"<repo-path>\") => <repository>
(exo:repo) => <repository>

(exo:clone :<codeberg/github> \"<owner>/<repo>\" :branch \"<value>\" \"<newrepo-path>\")

(exo:list &optional :<compact/modules> \"<repo-path>\") => <compact list>/<modules list>

(exo:verify &optional \"<repo-path>\")

(exo:clean &optional \"<repo-path>\") => <repository>
(exo:clean &optional \"<repo-path>\" :all) => <repository>

(exo:search :<mod-id> &optional \"<mod-version>\" \"<repo-path>\") => <modules list>

(exo:install \"<mod-cfg-path>\") => <module>
(exo:install :<codeberg/github> \"<owner>/<repo>\" :branch \"<value>\" :<mod-id> \"<mod-version>\") => <module>

(exo:remove :<mod-id> &optional \"<mod-version>\")

(exo:sign \"<mod-cfg-path>\" \"<key-sec-path>\" &optional \"key-sec-pass\")
(exo:sign (:<mod-id> \"<mod-ver\") \"<key-sec-path>\" &optional \"key-sec-pass\")

(exo:sign-verify \"<mod-cfg-path>\")
(exo:sign-verify (:<mod-id> \"<mod-ver\"))

(exo:token :<codeberg/github>) => \"<token>\"
(exo:token :<codeberg/github> \"<token>\") => \"<token>\"

(exo:interactive) => value
(exo:interactive t) => value

(exo:suppress-output) => value
(exo:suppress-output t) => value

(exo:check-utils)
(exo:version)
(exo:help)")

(defun exo:help ()
	(pfmtl %help% (exo:version)))