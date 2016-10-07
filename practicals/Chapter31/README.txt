The file css.lisp contains support for generating CSS output. To ues it add

  (:file "css" :depends-on ("packages" "html"))

to html.asd and export :define-css-macro, :css, and :emit-css in packages.lisp.

The file embed-foo-with-conditions-and-restarts.lisp contains the code discussed
in the sidebar in Chatper 30.
