(defsystem "lake-bin"
    :defsystem-depends-on (:deploy)
    :build-operation "deploy-op"
    :build-pathname "lake"
    :entry-point "lake/main:uiop-main"
    :depends-on ("lake/main"))
