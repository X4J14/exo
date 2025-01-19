### Exo (dependency managment tool (DMT) for Common Lisp)
```
  ▞▚▚▚▚▚▚▚▚▖▖▖  ▗▗▗▞▞▞▞▞▞▞▞▚ D
 ▞  ▚▚    ▚▚  ▚▞  ▞▞    ▞▞  ▚ M
▞    ▚▚▚▚  ▚▚    ▞▞    ▞▞    ▚ T
▚    ▞▞▞▞  ▞▞    ▚▚    ▚▚    ▞
 ▚  ▞▞    ▞▞  ▞▚  ▚▚    ▚▚  ▞
  ▚▞▞▞▞▞▞▞▞▘▘▘  ▝▝▝▚▚▚▚▚▚▚▚▞
```
**;; Key features ;;**

1) Auto packaging - no need to use (defpackage) at all.
2) Import/export symbols has 2 layers - internal and external.
3) Local and remote modules/repositories.
4) Using selfhosted server is optional - installing a code from codeberg.org and github.com directly.
5) Secure repositories - only signed code by owner can be installed and runned. (Optional)

```                                                                                                 
                         Host  │  codeberg.org                   │  github.com                   
                               │                                 │                               
┌────────────┐ ┌────────────┐  │  ┌────────────┐                 │                               
│ Repository │ │ Repository │  │  │ Repository │                 │                               
│ ┌────────┐ │ │ ┌────────┐ │  │  │ ┌────────┐ │ ┌────────────┐  │                 ┌────────────┐
│ │Module X│ │ │ │Module A├─┼──┼──┼─┤Module A│ │ │ Repository │  │                 │ Repository │
│ ├────────┤ │ │ ├────────┤ │  │  │ ├────────┤ │ │ ┌────────┐ │  │                 │ ┌────────┐ │
│ │Module Y│ │ │ │Module B├─┼──┼──┼─┤Module B│ │ │ │Module C│ │  │  ┌────────────┐ │ │Module E│ │
│ ├────────┤ │ │ ├────────┤ │  │  │ └────────┘ │ │ └────┬───┘ │  │  │  Module D  │ │ └───┬────┘ │
│ │Module E│ │ │ │Module C├─┼─┐│  └────────────┘ └──────┼─────┘  │  └─────┬──────┘ └─────┼──────┘
│ └────┬───┘ │ │ ├────────┤ │ └┼────────────────────────┘        │        │              │       
└──────┼─────┘ │ │Module D├─┼──┼─────────────────────────────────┼────────┘              │       
       │       │ └────────┘ │  │                                 │                       │       
       │       └────────────┘  │                                 │                       │       
       └───────────────────────┼─────────────────────────────────┼───────────────────────┘       
                               │                                 │                               
                                                                                app.monosketch.io
```
**;; Requirements ;;**
```
1) SBCL (tested on SBCL 2.4 and above)   https://sbcl.org
2) curl (used for fetching remote files) https://curl.se
3) signify (used for sign modules)       https://man.openbsd.org/signify
4) git (used for hashing files)          https://git-scm.com
```
*Exo implemented in 1 file without external dependencies except mentioned.*

**;; Installation ;;**

; As standalone file

1) Download last version "exo.lisp"
2) Run:
```lisp
(require :exo "<path-to-file>")
; or
(load "<path-to-file>")
```
; From Quicklisp
```lisp
; TODO
```
; From Ultralisp
```lisp
; TODO
```

**;;; Repository ;;;**

File structure:

```
/repository
	/module
		/version
			/info ; License and documentation files. (optional)
			/data ; Necessary files for the module. (optional)
			/src
				package.lisp
				...
			exo.mod
		...
	...
	exo.repo
```
; Example
```
~/.exo
	/alpha
		/1.0
			/info
				LICENSE
				readme.txt
			/src
				alpha.lisp
				beta.lisp
				/test
					gamma.lisp
			exo.mod
	/omega
		/0.1a
			/data
				test-1
				test-2
			/src
				omega.lisp
			exo.mod
	exo.repo
```
**Configuration file: exo.repo**
```lisp
(
	:exo-version 1.0
	:name "<value>" ; optional
	:description "<value>" ; optional
	:repository (
		:secure t|nil ; optional
		:sources ( ; optional
			:<github/codeberg> (
				:<source-id> "<owner>/<repo>" :branch "<value>"
				...
			)
			...
		)
		:signatures ( ; optional
			(:key "<value>"
				; optional in free form below
				:owner "<value>" 
				:url "<value>" ...)
			...
		))
	:about ( ; optional
		...
	))
```
**;; Commands ;;**

```lisp
; Create repository and set it current
(exo:repo "<repo-path>" :create) ;=> <repository>

; Set repository
(exo:repo "<repo-path>") ;=> <repository>

; Get current repository 
(exo:repo) ;=> <repository>

; Clone repository
(exo:clone :<codeberg/github> "<owner>/<repo>" :branch "<value>" "<new-repo-path>")

; Examples:
(exo:clone :codeberg "owner/my-exo-repo" :branch "prime" "~/.exo-clone-repo")

; Clean repository
(exo:clean &optional "<repo-path>") ;=> <repository>

; List all repository modules
(exo:list &optional :<compact/modules> "<repo-path>") ;=> <compact list>/<modules list>

; Verify repository
(exo:verify &optional "<repo-path>")

; Search modules in repository
(exo:search :<mod-id> &optional "<mod-version>" "<repo-path>") ;=> <modules list>

; Examples:
(exo:search :alpha)
(exo:search :alpha "1.0")
(exo:search :omega "0.1a" "~/.exo")

; Install local module to repository
(exo:install "<mod-cfg-path>") ;=> <module>

; Examples:
(exo:install "~/my-module/exo.mod") 

; Install remote module to repository
(exo:install :<codeberg/github> "<owner>/<repo>" :branch "<value>" :<mod-id> "<mod-version>") ;=> <module>

; Examples:
(exo:install :codeberg "owner/my-exo-repo" :branch "main" :alpha "1.0")
(exo:install :github "owner/my-exo-mod" :branch "master" :omega "0.1a")

; Remove module from repository
(exo:remove :<mod-id> "<mod-version>")

```
**;; Secure repository ;;**

When secure mode is enabled `(:secure t)`, then property `(:signatures (...))` should be defined also.
Secure mode guarantees that only signed modules by predefined keys can be installed or runned.
In other words, install and run either with unsigned modules or modules signed by unknown keys will forbidden.

Examples:
```lisp
(
	:exo-version 1.0s
	:name "Alpha"
	:description "Prime Alpha repository"
	:repository (
		:secure t
		:sources (
			:github (
				:alpha-repo "user/alpha-repo" :branch "main")
			:codeberg (
				:omega-mod "user/omega-mod" :branch "last")
		)
		:signatures ((
			:key "RWTVUUc2t+JbvfzMB+OX3sBhqWvrHuikwvJXE1sgMVXlnQLLiRgyQ00c"
			:owner "User"
			:url "https://codeberg.org/user/repo/raw/branch/main/key.pub")
		)))
```
;;; Module ;;;

**; Configuration file: exo.mod**
```lisp
(
	:exo-version 1.0
	:name "<value>"
	:description "<value>"
	:module (
		:id :<value>
		:version "<value>"
		:run "<value>"
		:run-... "<value>"
		...
		:test "<value>"
		:test-... "<value>"
		...
		:use (:<runtime-package> ...) or ((:<runtime-package> :<shadow-symbol> ...) ...)
		:dependencies (
			:<module-id> "<module-version"
			...
		)
		:signature (
			:key "<value>"
			:urls (
				"https://...<name>.pub"
				...
			)
		))
		:properties (
			:<key> "<value>"
			...
		)
		:about (
			...
		))
```
**; Minimal configuration file: exo.mod**

All properties are optional except mentiond below:
```lisp
(
	:exo-version 1.0
	:module (
		:id :<value>
		:version "<value>"
	))
```
**;; Packages hierarchy and managment ;;**

; Main principles

- The one lisp file of module represents one unique package.
- Path to the source file determines the package name.
- Never need to use (defpackage) at all.

Each source file managed by Exo should started with the "exo" function:
```lisp
(exo
	(:export
		:intern
			#:<symbol> ...)
		:extern
			#:<symbol> ...)
	(:import
		:intern
			#:<package>
			(#:<package> #:<nickname>)
			(#:<package> (#:<symbol>...))
		:extern
			...
		:runtime
			...)
	)) 
;...
(module code)
;...
```
*Note:
The :import block must be placed always below the :export block when the latter is specified.*

**;; Package types (relations) ;;**

1) internal - from the same module
2) external - from the external module
3) runtime - ordinary packages created before Exo was runned

**;; Export/import from/to module ;;**

Export and import has 2 main layers:
1) intern(al) - symbols will available within the same module
2) extern(al) - symbols will available for any external modules within repository

Additionally, :import provides :runtime key to import ordinary packages from runtime.

Example:
Let say, "alpha.lisp" from "alpha" module exports "hello" function.

**Repository structure:**
```
~/.exo ; repository directory
	/alpha  ; matches to module id
		/1.0 ; mathes to module version
			/src ; all source files always here
				alpha.lisp ; package "alpha" ; possible shorthand of "alpha/alpha" when the file name the same as module id
				beta.lisp  ; package "alpha/beta"
				/test
					gamma.lisp ; package "alpha/test/gamma"
			exo.mod ; module config file
	/omega
		/0.1a
			/src
				omega.lisp ; package "omega"
			exo.mod
	exo.repo : repository config file
```
~/.exo/alpha/1.0/exo.mod
```lisp
(
	:exo-version 1.0
	:module (
		:id :alpha
		:version "1.0"
		...
	))
```
~/.exo/alpha/1.0/src/alpha.lisp
```lisp
(exo
	(:export
		:intern #:hello
		:extern #:hello
	))
 
(defun hello () (print "Hello"))
```
**; Import to package of the same module**

~/.exo/alpha/1.0/src/beta.lisp
```lisp
(exo
	(:import
		:intern
			#:alpha  ; import all symbols => (hello)
			; or
			(#:alpha #:a) ; add local nickname => (a:hello)
			; or
			(#:alpha (#:hello)) ;  import specified symbols => (hello)
		))
```
**; Import to package from external module**

~/.exo/omega/0.1a/exo.mod
```lisp
(
	:exo-version 1.0
	:module (
		:id :omega
		:version "0.1a"
		...
		:dependencies (
			:alpha "1.0"
		)
	))
```
~/.exo/omega/0.1a/src/omega.lisp
```lisp
(exo
	(:import
		:extern
			... the same as above in beta.lisp ...
		))
```
**;; Install module. Local and remote dependencies ;;**

To do anything, Exo requires to set repository first.
```lisp
(exo:repo "<path>" :create) ; :create if repository does not exist
```

After that install module:

**; Local**
```lisp
(exo:install "<mod-cfg-path>")
```
The command will create module directory in the repository:
```
/<repo-path>
	/<mod-id> ; from module config file
		/<mod-version> ; from module config file
			/src
				.. copy of the source files ..
			exo.mod ; copy of the "<mod-cfg-path>"
```
**; Remote**
Remote installation make the same thing, but first download files to temporary directory of repository.
```lisp
(exo:install :codeberg/github "<owner>/<repo>" :branch "<value>" :<mod-id> "<mod-version>") ;=> <module>
```

The :sources property relates to remote modules/repositories only.
But :dependencies can contains data about local and remote modules.
```lisp
(
	:exo-version 1.0
	:module (
		...
		:dependencies (
			:<mod-id> "<mod-version>" ; value is string
			...
		))

; Example:
(
	:exo-version 1.0
	:module (
		...
		:dependencies (
			:gamma "0.99" ; local dependency
			:alpha-repo ( ; multiple modules in remote repository
				:alpha "1.0"
				:alpha-test "1.2")
			:omega-mod
				:omega "0.1a") ; only one possible because it is remote module (not repository)
		)))
```
*Note:
Remote installation can requires a token to access to private repositories or to avoid download limitation (for github)*
So, before to install run a commamd:
```lisp
(exo:token :<codeberg/github> "<value>")
```

**;; Running module ;;**

Module can be runnable as application.

To make module runnable requires:
1) Run property must be defined.
2) Run function must be defined and exported.

Example:
```lisp
(exo (:export :intern #:exo-run)
(define exo-run (mod-path mod-props)
	...)
```
*Note: Exo search a run function in the :intern export layer.*

**; Run and test parameters**
```lisp
(
	:exo-version 1.0
	:module (
		...
		:run :<package> ; by default will call "exo-run" function
		:run :<package>@<function> ; define custom function
		:run (:<package>@<function> args...) ; define custom function with args
		:run :@<function> ; Shorthand version starts with @. A package name the same as module id.
		:run (:@<function> args...)
		:run-<whatever> ...

		:test .. ; the same as above
		:test-<whatever> ...
		...
	))
```
**; Run module in standalone mode (by configuration file)**

In standalone mode the module runs without repository.
This mode is aimed to development process of modules.
When module created its install to repository and run in repository mode.
```lisp
(exo:run "<mod-cfg-path>") => <module> ; by default used :run parameter and "exo-run" function
(exo:run "<mod-cfg-path>" :<run-or-test>)  => <module> ; use custom parameter to run

; Example:
(exo:run "~/alpha-mod/exo.mod")
(exo:run "~/alpha-mod/exo.mod" :run-whatever)
(exo:run "~/alpha-mod/exo.mod" (:package 1 2 3)) ; will call "exo-run" by default
(exo:run "~/alpha-mod/exo.mod" (:package@run-all 1 2 3)) ; "run-all" function specified
(exo:run "~/alpha-mod/exo.mod" (:@run-all 1 2 3)) ; Shorthand version.
(exo:run "~/.exo/alpha/1.0/exo.mod") ; this is possible but repository and its settings will be ignored

**; Run module in repository mode**

(exo:run (:<mod-id> "<mod-version>") => <module>
(exo:run (:<mod-id> "<mod-version>") :<run-or-test>)  => <module>

Example:
(exo:run (:alpha "1.0"))
(exo:run (:omega "0.1a") :test)
(exo:run (:omega "0.1a") (:package 1 2 3)) ; will call "exo-run" by default
(exo:run (:omega "0.1a") (:package@test-all 1 2 3)) ; "run-all" function specified
(exo:run (:omega "0.1a") (:@test-all 1 2 3)) ; Shorthand version.
```
**;; Signing module ;;**

Signing the module creates ".hashes.sig" in the root module directory which contains hashes of all module files.
Before, need to create secure and public keys using "signify" tool from OpenBSD project.
The most popular Linux and BSD operation systems has this one in their repositories.

First, to generate key pairs need to run a command described below, save secure key to safe place and remember a password.
signify -G -p key.pub -s key.sec
Second, set string value of public key to `(:signature (:key "..."))` property.
Property `(:signature ( :links (...))` is optional, its purpose is to identify the key owner and it activates comparing the keys while signing.
If the keys mismatched then an error will be thrown.

Example:
```lisp
(
	:exo-version 1.0
	:module (
		...
		:signature (
			:key "RWTVUUc2t+JbvfzMB+OX3sBhqWvrHuikwvJXE1sgMVXlnQLLiRgyQ00c"
			:urls (
				"https://codeberg.org/user/repo/raw/branch/main/key.pub"
			))
	))
```
Exo "sign" command uses "signify", so, this one should be available for run.
Make sure that the util installed to folder mentioned in system PATH variable.
```lisp
(exo:sign "<mod-cfg-path>" "<secure-key-path>" "<secure-key-pass>") => <<module-dir>/.hashes.sig>
```
**;; Verifing module ;;**

To provide security EXO verifies signed modules while installing and before running.

**; Verify module manually**
```lisp
(exo:sign-verify "<mod-cfg-path>")
(exo:sign-verify (:<mod-id> "<mod-version"))
```

**;; Misc ;;**

**; Interactive mode**

 When enabled, the exo-error print message to standard output, otherwise printed debug stacktrace.
 Default is true
```lisp
(exo:interactive) => value
(exo:interactive t) => value
```
**; Suppress output**

 Suppress print anything to standard output.
 Default is false
```lisp
(exo:suppress-output) => value
(exo:suppress-output t) => value
```

**; Check utils**

Check existance of utils (`curl` and `signify`).

```lisp
(exo:check-utils)
```

**;; Disclaimer ;;**
```
; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```
