(require :asdf)

(load (make-pathname :defaults *load-pathname* :directory (butlast (pathname-directory *load-pathname*)) :name "setup"))

(asdf:operate 'asdf:load-op :wuwei)
