;;; puppet-mode.el --- A simple mode for editing puppet manifests

;; Copyright (C) 2011 Puppet Labs Inc

;; Author: Russ Allbery <rra@stanford.edu>
;; Maintainer: <info@puppetlabs.com>

;; This file is not part of GNU Emacs.

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; A simple mode for editing puppet manifests.

;;; Code:

(require 'align)

(defconst puppet-mode-version "0.2")

(defvar puppet-mode-abbrev-table nil
  "Abbrev table in use in puppet-mode buffers.")

(define-abbrev-table 'puppet-mode-abbrev-table ())

(defcustom puppet-indent-level 2
  "*Indentation of Puppet statements."
  :type 'integer :group 'puppet)

(defcustom puppet-include-indent 2
  "*Indentation of continued Puppet include statements."
  :type 'integer :group 'puppet)

(defvar puppet-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j"     'newline-and-indent)
    (define-key map "\C-m"     'newline-and-indent)
    (define-key map "\C-c\C-c" 'compile)
    (define-key map "\C-c\C-q" 'puppet-indent-and-align-resource)
    map)
  "Key map used in puppet-mode buffers.")

(defvar puppet-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\' "\"'"  table)
    (modify-syntax-entry ?\" "\"\"" table)
    (modify-syntax-entry ?#  "<"    table)
    (modify-syntax-entry ?\n ">#"   table)
    (modify-syntax-entry ?\\ "\\"   table)
    (modify-syntax-entry ?$  "'"    table)
    (modify-syntax-entry ?-  "_"    table)
    (modify-syntax-entry ?:  "_"    table)
    (modify-syntax-entry ?>  "."    table)
    (modify-syntax-entry ?=  "."    table)
    (modify-syntax-entry ?\; "."    table)
    (modify-syntax-entry ?\( "()"   table)
    (modify-syntax-entry ?\) ")("   table)
    (modify-syntax-entry ?\{ "(}"   table)
    (modify-syntax-entry ?\} "){"   table)
    (modify-syntax-entry ?\[ "(]"   table)
    (modify-syntax-entry ?\] ")["   table)
    table)
  "Syntax table in use in puppet-mode buffers.")

(defcustom puppet-indent-tabs-mode nil
  "*Indentation can insert tabs in puppet mode if this is non-nil."
  :type 'boolean :group 'puppet)

(defcustom puppet-comment-column 32
  "*Indentation column of comments."
  :type 'integer :group 'puppet)

(defun puppet-count-matches (re start end)
  "The same as Emacs 22 count-matches, for portability to other versions
of Emacs."
  (save-excursion
    (let ((n 0))
      (goto-char start)
      (while (re-search-forward re end t) (setq n (1+ n)))
      n)))

(defcustom puppet-manifest-verify-command "puppet parser validate --color=no"
  "Command to use when checking a manifest syntax"
  :type 'string :group 'puppet)

(defun puppet-comment-line-p ()
  "Return non-nil iff this line is a comment."
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (looking-at (format "\\s-*%s" comment-start)))))

(defun puppet-block-indent ()
  "If point is in a block, return the indentation of the first line of that
block (the line containing the opening brace).  Used to set the indentation
of the closing brace of a block."
  (save-excursion
    (save-match-data
      (let ((opoint (point))
            (apoint (search-backward "{" nil t)))
        (when apoint
          ;; This is a bit of a hack and doesn't allow for strings.  We really
          ;; want to parse by sexps at some point.
          (let ((close-braces (puppet-count-matches "}" apoint opoint))
                (open-braces 0))
            (while (and apoint (> close-braces open-braces))
              (setq apoint (search-backward "{" nil t))
              (when apoint
                (setq close-braces (puppet-count-matches "}" apoint opoint))
                (setq open-braces (1+ open-braces)))))
          (if apoint
              (current-indentation)
            nil))))))

(defun puppet-in-array ()
  "If point is in an array, return the position of the opening '[' of
that array, else return nil."
  (save-excursion
    (save-match-data
      (let ((opoint (point))
            (apoint (search-backward "[" nil t)))
        (when apoint
          ;; This is a bit of a hack and doesn't allow for strings.  We really
          ;; want to parse by sexps at some point.
          (let ((close-brackets (puppet-count-matches "]" apoint opoint))
                (open-brackets 0))
            (while (and apoint (> close-brackets open-brackets))
              (setq apoint (search-backward "[" nil t))
              (when apoint
                (setq close-brackets (puppet-count-matches "]" apoint opoint))
                (setq open-brackets (1+ open-brackets)))))
          apoint)))))

(defun puppet-in-include ()
  "If point is in a continued list of include statements, return the position
of the initial include plus puppet-include-indent."
  (save-excursion
    (save-match-data
      (let ((include-column nil)
            (not-found t))
        (while not-found
          (forward-line -1)
          (cond
           ((bobp)
            (setq not-found nil))
           ((looking-at "^\\s-*include\\s-+.*,\\s-*$")
            (setq include-column
                  (+ (current-indentation) puppet-include-indent))
            (setq not-found nil))
           ((not (looking-at ".*,\\s-*$"))
            (setq not-found nil))))
        include-column))))

(defun puppet-indent-line ()
  "Indent current line as puppet code."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (align-current)                     ; Always align attributes
    (if (bobp)
        (indent-line-to 0)              ; First line is always non-indented
      (let ((not-indented t)
            (array-start (puppet-in-array))
            (include-start (puppet-in-include))
            (block-indent (puppet-block-indent))
            cur-indent)
        (cond
         (array-start
          ;; This line probably starts with an element from an array.
          ;; Indent the line to the same indentation as the first
          ;; element in that array.  That is, this...
          ;;
          ;;    exec {
          ;;      "add_puppetmaster_mongrel_startup_links":
          ;;      command => "string1",
          ;;      creates => [ "string2", "string3",
          ;;      "string4", "string5",
          ;;      "string6", "string7",
          ;;      "string3" ],
          ;;      refreshonly => true,
          ;;    }
          ;;
          ;; ...should instead look like this:
          ;;
          ;;    exec {
          ;;      "add_puppetmaster_mongrel_startup_links":
          ;;      command => "string1",
          ;;      creates => [ "string2", "string3",
          ;;                   "string4", "string5",
          ;;                   "string6", "string7",
          ;;                   "string8" ],
          ;;      refreshonly => true,
          ;;    }
          (save-excursion
            (goto-char array-start)
            (forward-char 1)
            (re-search-forward "\\S-")
            (forward-char -1)
            (setq cur-indent (current-column))))
         (include-start
          (setq cur-indent include-start))
         ((and (looking-at "^\\s-*},?\\s-*$") block-indent)
          ;; This line contains a closing brace or a closing brace followed
          ;; by a comma and we're at the inner block, so we should indent it
          ;; matching the indentation of the opening brace of the block.
          (setq cur-indent block-indent))
         ((looking-at "^\\s-*}\\s-*\\(else\\|elsif\\)")
          ;; This line contains a closing brace and the else or elsif
          ;; keywords, so we should indent it matching the indentation of the
          ;; opening brace of the block.
          (setq cur-indent block-indent))
         ((looking-at "^[^\n\({]*\)\\s-*\\(inherits\\s-+[a-zA-Z0-9_:-]*\\s-*\\)?{\\s-*$")
          ;; Closing paren, optionally followed by the inherits keyword and a
          ;; class name and another brace will be indented one level too
          ;; much.
          (setq cur-indent (- (current-indentation) puppet-indent-level))
          (setq not-indented nil))
         (t
          ;; Otherwise, we did not start on a block-ending-only line.
          (save-excursion
            ;; Iterate backwards until we find an indentation hint
            (while not-indented
              (forward-line -1)
              (cond
               ;; Comment lines are ignored unless we're at the start of the
               ;; buffer.
               ((puppet-comment-line-p)
                (if (bobp)
                    (setq not-indented nil)))

               ;; Indent by one level more if the line ends with an open
               ;; brace after the else or elsif keywords.
               ((looking-at "^.*\\(else\\|elsif.*\\)\\s-*{\\s-*$")
                (setq cur-indent (+ (current-indentation) puppet-indent-level))
                (setq not-indented nil))

               ;; Indent by one level more if the line ends with an open
               ;; brace after the inherits keyword.
               ((looking-at "^[^\n\({]*\)\\s-*\\(inherits\\s-+[a-zA-Z0-9_:-]*\\s-*\\)?{\\s-*$")
                (setq cur-indent (+ (current-indentation) puppet-indent-level))
                (setq not-indented nil))

               ;; Brace or paren on a line by itself will already be indented
               ;; to the right level, so we can cheat and stop there.
               ((looking-at "^\\s-*[\)}]\\s-*")
                (setq cur-indent (current-indentation))
                (setq not-indented nil))

               ;; Brace (possibly followed by a comma) or paren not on a line
               ;; by itself will be indented one level too much, but don't
               ;; catch cases where the block is started and closed on the
               ;; same line.
               ((looking-at "^[^\n\({]*[\)}],?\\s-*$")
                (setq cur-indent (- (current-indentation) puppet-indent-level))
                (setq not-indented nil))

               ;; Indent by one level more than the start of our block. We
               ;; lose if there is more than one block opened and closed on
               ;; the same line but it's still unbalanced; hopefully people
               ;; don't do that.

               ((looking-at "^.*{[^\n}]*$")
                (setq cur-indent (+ (current-indentation) puppet-indent-level))
                (setq not-indented nil))

               ;; Indent by one level if the line ends with an open paren.
               ((looking-at "^.*\(\\s-*$")
                (setq cur-indent (+ (current-indentation) puppet-indent-level))
                (setq not-indented nil))

               ;; Semicolon ends a block for a resource when multiple
               ;; resources are defined in the same block, but try not to get
               ;; the case of a complete resource on a single line wrong.
               ((looking-at "^\\([^'\":\n]\\|\"[^\n\"]*\"\\|'[^\n']*'\\)*;\\s-*$")
                (setq cur-indent (- (current-indentation) puppet-indent-level))
                (setq not-indented nil))

               ;; Indent an extra level after : since it introduces a
               ;; resource.
               ((looking-at "^.*:\\s-*$")
                (setq cur-indent (+ (current-indentation) puppet-indent-level))
                (setq not-indented nil))

               ;; Start of buffer.
               ((bobp)
                (setq not-indented nil)))))

          ;; If this line contains only a closing paren, we should lose one
          ;; level of indentation.
          (if (looking-at "^\\s-*\)\\s-*$")
              (setq cur-indent (- cur-indent puppet-indent-level)))))

        ;; We've figured out the indentation, so do it.
        (if (and cur-indent (> cur-indent 0))
            (indent-line-to cur-indent)
          (indent-line-to 0)))))

  ;; Set cursor position to end of line for all-whitespace lines
  ;; or start of text if we're in the indent region
  (if (looking-back "^\\s-*")
      (if (looking-at "\\s-*$")
          (end-of-line)
        (back-to-indentation))))

(defun puppet-mode-compilation-buffer-name (&rest ignore)
  "Return the name of puppet compilation buffer"
  "*puppet-lint*")

(defun puppet-indent-and-align-resource ()
  "Indent and align the current puppet resource declaration."
  (interactive "*")
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))
    (align (region-beginning) (region-end))))

(defvar puppet-font-lock-syntax-table
  (let* ((tbl (copy-syntax-table puppet-mode-syntax-table)))
    (modify-syntax-entry ?_ "w" tbl)
    tbl))

(defvar puppet-font-lock-keywords
  (list
   ;; defines, classes, and nodes
   '("^\\s *\\(class\\|define\\|node\\)\\s +\\([^{( \t\n]+\\)"
     2 font-lock-function-name-face)
   ;; inheritence
   '("\\s +inherits\\s +\\([^( \t\n]+\\)"
     1 font-lock-function-name-face)
   ;; include & require
   '("\\(^\\|\\s +\\)\\(include\\|require\\)\\s +\\(\\([a-zA-Z0-9:_-]+\\(,[ \t\n]*\\)?\\)+\\)"
     3 font-lock-reference-face)
   ;; constants
   '("\\(^\\|[^_:.@$]\\)\\b\\(true\\|false\\|undef\\)\\>"
     2 font-lock-constant-face)
   ;; parameters with hash rockets
   ;; (borrow the preprocessor face for this case)
   '("\\([a-z][a-z0-9_]*\\)[ \t\n]*=>"
     1 font-lock-preprocessor-face)
   ;; values for parameter 'ensure' with hash rocket
   (list (concat "ensure[ \t\n]*=>[ \t\n]*"
                 (regexp-opt '("absent"
                               "configured"
                               "defined"
                               "directory"
                               "false"
                               "file"
                               "held"
                               "installed"
                               "latest"
                               "link"
                               "mounted"
                               "no_shutdown"
                               "present"
                               "purged"
                               "role"
                               "running"
                               "shutdown"
                               "stopped"
                               "true"
                               "unmounted") 'words))
         1 'font-lock-constant-face)
   ;; variables
   '("\\$[a-zA-Z0-9_:]+"
     0 font-lock-variable-name-face)
   ;; keywords (important)
   (list (regexp-opt
          '("alert"
            "crit"
            "debug"
            "emerg"
            "err"
            "fail"
            "info"
            "notice"
            "warning") 'symbols)
         0 'font-lock-warning-face)
   ;; keywords (normal)
   (list (regexp-opt
          '("and"
            "case"
            "class"
            "contain"
            "create_resources"
            "default"
            "define"
            "defined"
            "each"
            "else"
            "elsif"
            "epp"
            "err"
            "extlookup"
            "false"
            "file"
            "filebucket"
            "filter"
            "fqdn_rand"
            "generate"
            "hiera"
            "hiera_array"
            "hiera_hash"
            "hiera_include"
            "if"
            "import"
            "in"
            "include"
            "inherits"
            "inline_epp"
            "inline_template"
            "lookup"
            "map"
            "md5"
            "node"
            "or"
            "realize"
            "reduce"
            "regsubst"
            "require"
            "search"
            "sha1"
            "shellquote"
            "slice"
            "split"
            "sprintf"
            "tag"
            "tagged"
            "template"
            "true"
            "undef"
            "unless"
            "versioncmp")
          'symbols)
         1 'font-lock-keyword-face)
   ;; keywords (stdlib functions)
   (list (regexp-opt
          '("abs"
            "any2array"
            "assert_private"
            "base64"
            "basename"
            "bool2num"
            "bool2str"
            "capitalize"
            "ceiling"
            "chomp"
            "chop"
            "clamp"
            "concat"
            "convert_base"
            "count"
            "defined_with_params"
            "delete"
            "delete_at"
            "delete_undef_values"
            "delete_values"
            "difference"
            "dirname"
            "dos2unix"
            "downcase"
            "empty"
            "ensure_packages"
            "ensure_resource"
            "flatten"
            "floor"
            "fqdn_rand_string"
            "fqdn_rotate"
            "get_module_path"
            "getparam"
            "getvar"
            "grep"
            "has_interface_with"
            "has_ip_address"
            "has_ip_network"
            "has_key"
            "hash"
            "intersection"
            "is_a"
            "is_absolute_path"
            "is_array"
            "is_bool"
            "is_domain_name"
            "is_float"
            "is_function_available"
            "is_hash"
            "is_integer"
            "is_ip_address"
            "is_mac_address"
            "is_numeric"
            "is_string"
            "join"
            "join_keys_to_values"
            "keys"
            "load_module_metadata"
            "loadyaml"
            "lstrip"
            "max"
            "member"
            "merge"
            "min"
            "num2bool"
            "parsejson"
            "parseyaml"
            "pick"
            "pick_default"
            "prefix"
            "pw_hash"
            "range"
            "reject"
            "reverse"
            "rstrip"
            "seeded_rand"
            "shuffle"
            "size"
            "sort"
            "squeeze"
            "str2bool"
            "str2saltedsha512"
            "strftime"
            "strip"
            "suffix"
            "swapcase"
            "time"
            "to_bytes"
            "try_get_value"
            "type3x"
            "type_of"
            "union"
            "unique"
            "unix2dos"
            "upcase"
            "uriescape"
            "validate_absolute_path"
            "validate_array"
            "validate_augeas"
            "validate_bool"
            "validate_cmd"
            "validate_hash"
            "validate_integer"
            "validate_ip_address"
            "validate_numeric"
            "validate_re"
            "validate_slength"
            "validate_string"
            "validate_x509_rsa_key_pair"
            "values"
            "values_at"
            "zip")
          'symbols)
         1 font-lock-function-name-face)
   ;; usage of types
   '("^\\s *\\(@\\{,2\\}\\(::\\)?[a-z][a-zA-Z0-9_:-]*\\)\\s +{"
     1 font-lock-type-face)
   ;; overrides and type references
   '("\\(\\s +\\|\\s(\\)\\([A-Z][a-zA-Z0-9_:-]*\\)\\["
     2 font-lock-type-face)
   ;; general delimited string
   '("\\(^\\|[[ \t\n<+(,=]\\)\\(%[xrqQwW]?\\([^<[{(a-zA-Z0-9 \n]\\)[^\n\\\\]*\\(\\\\.[^\n\\\\]*\\)*\\(\\3\\)\\)"
     (2 font-lock-string-face)))
  "*Additional expressions to highlight in puppet mode.")

(defvar puppet-align-rules
  '((puppet-align
     (modes   . '(puppet-mode))
     (regexp  . "\\(\\s-*\\)=>\\(\\s-*\\)")
     (group   1 2)
     (spacing . 1)
     (repeat  . t)))
  "*Rules to align code in puppet mode.")

(defvar puppet-completion-words-global
  '("augeas" "class" "computer" "cron" "exec" "file" "filebucket" "group"
    "host" "interface" "k5login" "macauthorization" "mailalias" "maillist"
    "mcx" "mount" "nagios_command" "nagios_contact" "nagios_contactgroup"
    "nagios_host" "nagios_hostdependency" "nagios_hostescalation"
    "nagios_hostextinfo" "nagios_hostgroup" "nagios_service"
    "nagios_servicedependency" "nagios_serviceescalation"
    "nagios_serviceextinfo" "nagios_servicegroup" "nagios_timeperiod"
    "notify" "package" "resources" "router" "schedule" "scheduled_task"
    "selboolean" "selmodule" "service" "ssh_authorized_key" "sshkey" "stage"
    "tidy" "user" "vlan" "yumrepo" "zfs" "zone" "zpool"
    ;; metaparameter
    "alias" "audit" "before" "consume" "export" "loglevel" "noop" "notify"
    "require" "schedule" "stage" "subscribe" "tag")
  "Global keywords for completion in `puppet-mode'.")

(defvar puppet-completion-words-alist
  '(("augeas" . ("name" "changes" "context" "force" "incl" "lens" "load_path"
                 "onlyif" "provider" "returns" "root" "show_diff"
                 "type_check"))
    ("computer" . ("name" "ensure" "en_address" "ip_address" "provider"
                   "realname"))
    ("cron" . ("name" "ensure" "command" "environment" "hour" "minute"
               "month" "monthday" "provider" "special" "target" "user"
               "weekday"))
    ("exec" . ("command" "creates" "cwd" "environment" "group" "logoutput"
                 "onlyif" "path" "provider" "refresh" "refreshonly" "returns"
                 "timeout" "tries" "try_sleep" "umask" "unless" "user"))
    ("file" . ("path" "ensure" "backup" "checksum" "content" "ctime" "force"
                 "group" "ignore" "links" "mode" "mtime" "owner" "provider"
                 "purge" "recurse" "recurselimit" "replace"
                 "selinux_ignore_defaults" "selrange" "selrole" "seltype"
                 "seluser" "show_diff" "source" "source_permissions"
                 "sourceselect" "target" "type" "validate_cmd"
                 "validate_replacement"))
    ("filebucket" . ("name" "path" "port" "server"))
    ("group" . ("name" "ensure" "allowdupe" "attribute_membership"
                "attributes" "auth_membership" "forcelocal" "gid"
                "ia_load_module" "members" "provider" "system"))
    ("host" . ("name" "ensure" "comment" "host_aliases" "ip" "provider"
               "target"))
    ("interface" . ("name" "ensure" "allowed_trunk_vlans" "description"
                    "device_url" "duplex" "encapsulation" "etherchannel"
                    "ipaddress" "mode" "native_vlan" "provider" "speed"))
    ("k5login" . ("path" "ensure" "mode" "principals" "provider"))
    ("macauthorization" . ("name" "ensure" "allow_root" "auth_class"
                           "auth_type" "authenticate_user" "comment" "group"
                           "k_of_n" "mechanisms" "provider" "rule"
                           "session_owner" "shared" "timeout" "tries"))
    ("mailalias" . ("name" "ensure" "file" "provider" "recipient" "target"))
    ("maillist" . ("name" "ensure" "admin" "description" "mailserver"
                   "password" "provider" "webserver"))
    ("mcx" . ("name" "ensure" "content" "ds_name" "ds_type" "provider"))
    ("mount" . ("name" "ensure" "atboot" "blockdevice" "device" "dump"
                "fstype" "options" "pass" "provider" "remounts" "target"))
    ("nagios_command" . ("command_name" "ensure" "command_line" "group"
                         "mode" "owner" "poller_tag" "provider" "target"
                         "use"))
    ("nagios_contact" . ("contact_name" "ensure" "address1" "address2"
                         "address3" "address4" "address5" "address6" "alias"
                         "can_submit_commands" "contactgroups" "email"
                         "group" "host_notification_commands"
                         "host_notification_options"
                         "host_notification_period"
                         "host_notifications_enabled" "mode" "owner" "pager"
                         "provider" "register" "retain_nonstatus_information"
                         "retain_status_information"
                         "service_notification_commands"
                         "service_notification_options"
                         "service_notification_period"
                         "service_notifications_enabled" "target" "use"))
    ("nagios_contactgroup" . ("contactgroup_name" "ensure" "alias"
                              "contactgroup_members" "group" "members" "mode"
                              "owner" "provider" "register" "target" "use"))
    ("nagios_host" . ("host_name" "ensure" "action_url"
                      "active_checks_enabled" "address" "alias"
                      "business_impact" "check_command" "check_freshness"
                      "check_interval" "check_period" "contact_groups"
                      "contacts" "display_name" "event_handler"
                      "event_handler_enabled" "failure_prediction_enabled"
                      "first_notification_delay" "flap_detection_enabled"
                      "flap_detection_options" "freshness_threshold" "group"
                      "high_flap_threshold" "hostgroups" "icon_image"
                      "icon_image_alt" "initial_state" "low_flap_threshold"
                      "max_check_attempts" "mode" "notes" "notes_url"
                      "notification_interval" "notification_options"
                      "notification_period" "notifications_enabled"
                      "obsess_over_host" "owner" "parents"
                      "passive_checks_enabled" "poller_tag"
                      "process_perf_data" "provider" "realm" "register"
                      "retain_nonstatus_information"
                      "retain_status_information" "retry_interval"
                      "stalking_options" "statusmap_image" "target" "use"
                      "vrml_image"))
    ("nagios_hostdependency" . ("_naginator_name" "ensure"
                                "dependency_period" "dependent_host_name"
                                "dependent_hostgroup_name"
                                "execution_failure_criteria" "group"
                                "host_name" "hostgroup_name"
                                "inherits_parent" "mode"
                                "notification_failure_criteria" "owner"
                                "provider" "register" "target" "use"))
    ("nagios_hostescalation" . ("_naginator_name" "ensure" "contact_groups"
                                "contacts" "escalation_options"
                                "escalation_period" "first_notification"
                                "group" "host_name" "hostgroup_name"
                                "last_notification" "mode"
                                "notification_interval" "owner" "provider"
                                "register" "target" "use"))
    ("nagios_hostextinfo" . ("host_name" "ensure" "group" "icon_image"
                             "icon_image_alt" "mode" "notes" "notes_url"
                             "owner" "provider" "register" "statusmap_image"
                             "target" "use" "vrml_image"))
    ("nagios_hostgroup" . ("hostgroup_name" "ensure" "action_url" "alias"
                           "group" "hostgroup_members" "members" "mode"
                           "notes" "notes_url" "owner" "provider" "realm"
                           "register" "target" "use"))
    ("nagios_service" . ("_naginator_name" "ensure" "action_url"
                         "active_checks_enabled" "business_impact"
                         "check_command" "check_freshness" "check_interval"
                         "check_period" "contact_groups" "contacts"
                         "display_name" "event_handler"
                         "event_handler_enabled" "failure_prediction_enabled"
                         "first_notification_delay" "flap_detection_enabled"
                         "flap_detection_options" "freshness_threshold"
                         "group" "high_flap_threshold" "host_name"
                         "hostgroup_name" "icon_image" "icon_image_alt"
                         "initial_state" "is_volatile" "low_flap_threshold"
                         "max_check_attempts" "mode" "normal_check_interval"
                         "notes" "notes_url" "notification_interval"
                         "notification_options" "notification_period"
                         "notifications_enabled" "obsess_over_service"
                         "owner" "parallelize_check" "passive_checks_enabled"
                         "poller_tag" "process_perf_data" "provider"
                         "register" "retain_nonstatus_information"
                         "retain_status_information" "retry_check_interval"
                         "retry_interval" "service_description"
                         "servicegroups" "stalking_options" "target" "use"))
    ("nagios_servicedependency" . ("_naginator_name" "ensure"
                                   "dependency_period" "dependent_host_name"
                                   "dependent_hostgroup_name"
                                   "dependent_service_description"
                                   "execution_failure_criteria" "group"
                                   "host_name" "hostgroup_name"
                                   "inherits_parent" "mode"
                                   "notification_failure_criteria" "owner"
                                   "provider" "register"
                                   "service_description" "target" "use"))
    ("nagios_serviceescalation" . ("_naginator_name" "ensure"
                                   "contact_groups" "contacts"
                                   "escalation_options" "escalation_period"
                                   "first_notification" "group" "host_name"
                                   "hostgroup_name" "last_notification"
                                   "mode" "notification_interval" "owner"
                                   "provider" "register"
                                   "service_description" "servicegroup_name"
                                   "target" "use"))
    ("nagios_serviceextinfo" . ("_naginator_name" "ensure" "action_url"
                                "group" "host_name" "icon_image"
                                "icon_image_alt" "mode" "notes" "notes_url"
                                "owner" "provider" "register"
                                "service_description" "target" "use"))
    ("nagios_servicegroup" . ("servicegroup_name" "ensure" "action_url"
                              "alias" "group" "members" "mode" "notes"
                              "notes_url" "owner" "provider" "register"
                              "servicegroup_members" "target" "use"))
    ("nagios_timeperiod" . ("timeperiod_name" "ensure" "alias" "exclude"
                            "friday" "group" "mode" "monday" "owner"
                            "provider" "register" "saturday" "sunday"
                            "target" "thursday" "tuesday" "use" "wednesday"))
    ("notify" . ("name" "message" "withpath"))
    ("package" . ("provider" "name" "ensure" "adminfile" "allow_virtual"
                  "allowcdrom" "category" "configfiles" "description"
                  "flavor" "install_options" "instance" "package_settings"
                  "platform" "reinstall_on_refresh" "responsefile" "root"
                  "source" "status" "uninstall_options" "vendor"))
    ("resources" . ("name" "purge" "unless_system_user" "unless_uid"))
    ("router" . ("url"))
    ("schedule" . ("name" "period" "periodmatch" "range" "repeat" "weekday"))
    ("scheduled_task" .("name" "ensure" "arguments" "command" "enabled"
                        "password" "provider" "trigger" "user"
                        "working_dir"))
    ("selboolean" . ("name" "persistent" "provider" "value"))
    ("selmodule" . ("name" "ensure" "provider" "selmoduledir" "selmodulepath"
                    "syncversion"))
    ("service" . ("name" "ensure" "binary" "control" "enable" "flags"
                  "hasrestart" "hasstatus" "manifest" "path" "pattern"
                  "provider" "restart" "start" "status" "stop"))
    ("ssh_authorized_key" . ("name" "ensure" "key" "options" "provider"
                             "target" "type" "user"))
    ("sshkey" . ("name" "ensure" "host_aliases" "key" "provider" "target"
                 "type"))
    ("stage" . ("name"))
    ("tidy" . ("path" "age" "backup" "matches" "recurse" "rmdirs" "size"
               "type"))
    ("user" . ("name" "ensure" "allowdupe" "attribute_membership"
               "attributes" "auth_membership" "auths" "comment" "expiry"
               "forcelocal" "gid" "groups" "home" "ia_load_module"
               "iterations" "key_membership" "keys" "loginclass" "managehome"
               "membership" "password" "password_max_age" "password_min_age"
               "profile_membership" "profiles" "project" "provider"
               "purge_ssh_keys" "role_membership" "roles" "salt" "shell"
               "system" "uid"))
    ("vlan" . ("name" "ensure" "description" "device_url" "provider"))
    ("yumrepo" . ("name" "ensure" "assumeyes" "bandwidth" "baseurl" "cost"
                  "deltarpm_metadata_percentage" "deltarpm_percentage"
                  "descr" "enabled" "enablegroups" "exclude" "failovermethod"
                  "gpgcakey" "gpgcheck" "gpgkey" "http_caching" "include"
                  "includepkgs" "keepalive" "metadata_expire" "metalink"
                  "mirrorlist" "mirrorlist_expire" "priority" "protect"
                  "provider" "proxy" "proxy_password" "proxy_username"
                  "repo_gpgcheck" "retries" "s3_enabled"
                  "skip_if_unavailable" "sslcacert" "sslclientcert"
                  "sslclientkey" "sslverify" "target" "throttle" "timeout"))
    ("zfs" . ("name" "ensure" "aclinherit" "aclmode" "atime" "canmount"
              "checksum" "compression" "copies" "dedup" "devices" "exec"
              "logbias" "mountpoint" "nbmand" "primarycache" "provider"
              "quota" "readonly" "recordsize" "refquota" "refreservation"
              "reservation" "secondarycache" "setuid" "shareiscsi" "sharenfs"
              "sharesmb" "snapdir" "version" "volsize" "vscan" "xattr"
              "zoned"))
    ("zone" . ("name" "ensure" "autoboot" "clone" "create_args" "dataset"
               "id" "inherit" "install_args" "ip" "iptype" "path" "pool"
               "provider" "realhostname" "shares" "sysidcfg"))
    ("zpool" . ("pool" "ensure" "disk" "log" "mirror" "provider"
                "raid_parity" "raidz" "spare")))
  "Words to use for completion in `puppet-mode'.")

(defun puppet-completion-at-point ()
  "Complete the word at point."
  (save-excursion
    (save-match-data
      (let ((bounds (bounds-of-thing-at-point 'word))
            (intype (and (re-search-backward "\\_<\\([a-zA-Z0-9:_]+\\)\\_>\\s-*{" nil t)
                         (match-string-no-properties 1))))
        (when bounds
          (list (car bounds)
                (cdr bounds)
                (delete-dups
                 (append puppet-completion-words-global
                         (assoc intype puppet-completion-words-alist)))
                :exclusive 'no))))))

;;;###autoload
(defun puppet-mode ()
  "Major mode for editing puppet manifests.

The variable puppet-indent-level controls the amount of indentation.
\\{puppet-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map puppet-mode-map)
  (setq mode-name "Puppet")
  (setq major-mode 'puppet-mode)
  (set-syntax-table puppet-mode-syntax-table)
  (set (make-local-variable 'local-abbrev-table) puppet-mode-abbrev-table)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+ *")
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (set (make-local-variable 'comment-column) puppet-comment-column)
  (set (make-local-variable 'indent-line-function) 'puppet-indent-line)
  (set (make-local-variable 'indent-tabs-mode) puppet-indent-tabs-mode)
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'paragraph-start) "\f\\|[   ]*$\\|#$")
  (set (make-local-variable 'paragraph-separate) "\\([  \f]*\\|#\\)$")
  (set (make-local-variable 'open-paren-in-column-0-is-defun-start) t)
  (set (make-local-variable 'defun-prompt-regexp) "^\\s-*\\(class\\|define\\|node\\)\\s-+[a-z][^{]*")
  (or (boundp 'font-lock-variable-name-face)
      (setq font-lock-variable-name-face font-lock-type-face))
  (set (make-local-variable 'font-lock-keywords) puppet-font-lock-keywords)
  (set (make-local-variable 'font-lock-multiline) t)
  (set (make-local-variable 'font-lock-defaults)
       '((puppet-font-lock-keywords) nil nil))
  (set (make-local-variable 'font-lock-syntax-table)
       puppet-font-lock-syntax-table)
  (set (make-local-variable 'compilation-buffer-name-function) 'puppet-mode-compilation-buffer-name)
  (set (make-local-variable 'compile-command) (concat puppet-manifest-verify-command " " (buffer-file-name)))
  (dolist (ar puppet-align-rules) (add-to-list 'align-rules-list ar))
  (add-to-list 'completion-at-point-functions 'puppet-completion-at-point)
  (run-hooks 'puppet-mode-hook))

(provide 'puppet-mode)

;;; puppet-mode.el ends here
