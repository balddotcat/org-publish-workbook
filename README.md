
# org-publish-workbook

`org-export` can  easily generate  multiple versions  of the
same org  data; an **org-publish-workbook** binds  an export
backend  with an  `org-publish`  project, to  help with  the
process of iteratively customizing output.

A **workbook** consists of an entry in `org-publish-workbook
-alist`, a set of entries in `org-publish-project-alist` and
an `org-export-derived-backend`.

As  an  example,  the  initial outline  of  publishing  this
project's `README.md` consisted of the following setup.

    (org-publish-workbook-add
     'documentation
     '(md                            ;; workbook derived from markdown exporter (ox-md.el)
       :filters-alist                ;; word wrap paragraphs, with hyphens
       ((:filter-paragraph (lambda (content backend info)
    			 (with-temp-buffer
    			   (insert content)
    			   (modify-category-entry ?- ?|)
    			   (set-fill-column 60)
    			   (fill-region (point-min) (point-max) 'full t nil)
    			   (buffer-string)))))
       :exclude ".*"
       :base-extension ".md"))

    (org-publish-workbook-add-project
     'documentation
     '("org-publish-workbook"
       :base-directory "~/workbook/lib"
       :include ["org-publish-workbook.org"]
       :publishing-directory "~/workbook/lib/org-publish-workbook"
       :decorators ((:before-processing    ;; insert title as md export is body only
    		 (lambda ()
    		   (org-map-entries (lambda () (org-demote-subtree)) "LEVEL=1")
    		   (org-insert-heading)
    		   (insert "org-publish-workbook"))))))


## adding a workbook

Workbooks   are   initialized  with   `org-publish-workbook-
add`. This function adds  an entry to `org-publish-workbook-
alist`, based on the first argument, **label**.

The second argument, **backend**, can be either a symbol, or
a list starting  with a symbol and an alist  of options. The
symbol  should  reference  an  **org-export  backend**;  the
provided options are used  to create an `org-export-derived-
backend`.

Publish project specific  options `:base-directory`, `:base-
extension`, `:exclude`, `:publishing-directory`, `:template`
and `:template-directory`  can be also be  specified, to set
the workbook's projects' default values.

The rest  of the arguments  are used to  create **projects**
with `org-publish-workbook-add-project`.


### org-export backend

Each  workbook  creates   a  reusable  (further  extendable)
derived backend based on the original exporter, for use with
all of  it's projects. For  a list of options  and settings,
please  see the  doc  string of  `org-export-define-derived-
backend`.


### org-publish projects

When a project is added to  a workbook, it is added to `org-
publish-project-alist` labelled as `[workbook`-`project]`.

When not set directly, options default to values set in each
respective  **workbook**,  or their  equivalent  environment
variables;  root-directory  is  set to  the  Emacs  server's
initial working directory.

-   :template-directory; `.etc/export-template`
-   :output-directory; `.var`

Commonly  used   options  are  `:include`,   `:exclude`  and
`:recursive`  -   the  custom  properties   `:template`  and
`:decorate`   create    export   hooks    through   **output
decorators**.


## output decorators

By adding  **decorators** to  a publishing  project's export
pipeline,  the  `:final-output`,  or the  contents  of  each
buffer `:before-processing`, can be directly edited.

When editing  the `:final-output`, the function  supplied to
`workbook-decorate` is called with **contents** and optional
**args**. The  function's output becomes the  result of this
export action.

`:before-processing` hooks  are expanded within  the context
of each current-buffer, allowing for direct editing.

Each  hook is  activated during  a project's  `:preparation-
function`, and deactivated in it's `:completion-function`.


### template decorator

When a project's `:template` property  is set, by default, a
hook  is  run during  publishing  to  insert or  update  the
current   buffer's   `#+SETUPFILE`   property   before   any
processing takes place.

Alternately,  the   `insert-template-fn`  variable   can  be
updated to specify a function  which will be called with the
resolved path of the project's template file.


## rendering

The   `org-publish-workbook-render`  function   publishes  a
workbook's projects. Optional arguments are `force`, `async`
and projects - to publish only the ones specified.


## tests

    emacs -batch \
          -l ert \
          -l org-publish-workbook-tests.el \
          -f ert-run-tests-batch-and-exit

