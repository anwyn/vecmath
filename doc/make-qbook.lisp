(asdf:oos 'asdf:load-op :vecmath)
(asdf:oos 'asdf:load-op :qbook)

(asdf:oos 'qbook:publish-op :vecmath
          :generator (make-instance 'qbook:html-generator
                                    :title "Simple Vector Mathematics in Common Lisp"
                                    :output-directory "./html/"))

