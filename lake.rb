class Lake < Formula
  desc "Lake is a GNU make like build utility in Common Lisp."
  homepage "https://github.com/svetlyak40wt/lake"
  url "https://github.com/svetlyak40wt/lake/archive/v0.1.3.tar.gz"
  sha256 "bdefa5ab5d598707b59eec72d2d6123446d4061e41be71748f87053a7fd0ed12"
  head NIL

  depends_on "sbcl"

  resource "alexandria" do
    url "http://beta.quicklisp.org/archive/alexandria/2019-07-10/alexandria-20190710-git.tgz"
    sha256 "e0642bd6f8af8eb71e3359b45e11a135fa3c9a511492bc9dbbcd10ec7d694704"
  end

  resource "cffi" do
    url "http://beta.quicklisp.org/archive/cffi/2019-07-10/cffi_0.20.1.tgz"
    sha256 "6a427cc08f0418900bae8a76a690bb1c51fd61caf7efcb677d31701e0ce3ec5e"
  end

  resource "cl-babel-babel" do
    url "http://dist.ultralisp.org/ultralisp/archive/c/cl-babel-babel-20190618134909.tgz"
    sha256 "db6c1b6e21e95519dc5626cd0ab041d2036062178f021da68214b6cd1f00ee0a"
  end

  resource "edicl-cl-interpol" do
    url "http://dist.ultralisp.org/ultralisp/archive/e/edicl-cl-interpol-20190618141007.tgz"
    sha256 "28e3739133908fea3a4fa76aaaeb773d9bcacbdd9fd3aeda20577e08e3efb6d0"
  end

  resource "edicl-cl-ppcre" do
    url "http://dist.ultralisp.org/ultralisp/archive/e/edicl-cl-ppcre-20190618135807.tgz"
    sha256 "abddf50018b7d4593e962e6920e74cc5a08015bca29d8c79fc378466a30aa716"
  end

  resource "edicl-cl-unicode" do
    url "http://dist.ultralisp.org/ultralisp/archive/e/edicl-cl-unicode-20190618142709.tgz"
    sha256 "56a1c24e7a62c0f47da8cb4e62370ef480596cfdc42d8176fddf15cde040f9c3"
  end

  resource "edicl-flexi-streams" do
    url "http://dist.ultralisp.org/ultralisp/archive/e/edicl-flexi-streams-20190618143408.tgz"
    sha256 "7c5bf9b5106da9137b7a85d859a32eaf37c9b5f53f8d906ca4be6422bb6531e8"
  end

  resource "lmj-lparallel" do
    url "http://dist.ultralisp.org/ultralisp/archive/l/lmj-lparallel-20190319125038.tgz"
    sha256 "504d05c7b7b3c51d72d936dc00667184b21909e644e874a4e748a91017b13c11"
  end

  resource "m2ym-cl-syntax" do
    url "http://dist.ultralisp.org/ultralisp/archive/m/m2ym-cl-syntax-20190319100142.tgz"
    sha256 "555a992d9264633ce4b0eac2df5746148a446dfe411b0532c9f55fe8036f285d"
  end

  resource "m2ym-trivial-types" do
    url "http://dist.ultralisp.org/ultralisp/archive/m/m2ym-trivial-types-20190318184855.tgz"
    sha256 "0b05aab17b8ef14d7576ac554a2ee30e52d848e7c95dcd0c27d7a357c3110351"
  end

  resource "melisgl-named-readtables" do
    url "http://dist.ultralisp.org/ultralisp/archive/m/melisgl-named-readtables-20190319063736.tgz"
    sha256 "08a09925f6f73a582834d0516bef81fa1872dd2ec8febb0db26f84038531592c"
  end

  resource "sharplispers-split-sequence" do
    url "http://dist.ultralisp.org/ultralisp/archive/s/sharplispers-split-sequence-20190517073140.tgz"
    sha256 "1b0bc2fb49ec66b845c2a85a0dcf6f675c8cd0b7d283de1822401ef58df944a5"
  end

  resource "Shinmera-deploy" do
    url "http://dist.ultralisp.org/ultralisp/archive/S/Shinmera-deploy-20190911151713.tgz"
    sha256 "2d2d14412142ef3527a023cae53ffd3d802d9412150113105c83085075992eb0"
  end

  resource "Shinmera-documentation-utils" do
    url "http://dist.ultralisp.org/ultralisp/archive/S/Shinmera-documentation-utils-20190627101653.tgz"
    sha256 "f2a238459c2a91032af093487e8237b38c321eae2c656c23558b711f6bc90815"
  end

  resource "Shinmera-trivial-indent" do
    url "http://dist.ultralisp.org/ultralisp/archive/S/Shinmera-trivial-indent-20190819223127.tgz"
    sha256 "40aa939f547d28a6df1f815814ba4943428e5cbff288ffc4fb67f4a883c328ad"
  end

  resource "sionescu-bordeaux-threads" do
    url "http://dist.ultralisp.org/ultralisp/archive/s/sionescu-bordeaux-threads-20190618135307.tgz"
    sha256 "ca77a1ccc7eecf8927484b8cf8061efc8ecda3d26ef94c6579ef2be8d0d437a6"
  end

  resource "trivial-features-trivial-features" do
    url "http://dist.ultralisp.org/ultralisp/archive/t/trivial-features-trivial-features-20190709040636.tgz"
    sha256 "438ff741c756902c5878565201ad703ab775ce76ac8a085c4149e6ccc3e19060"
  end

  resource "trivial-gray-streams-trivial-gray-streams" do
    url "http://dist.ultralisp.org/ultralisp/archive/t/trivial-gray-streams-trivial-gray-streams-20190319050358.tgz"
    sha256 "6f85c53751a6c39897907582673b33ab25f2b2703f3b041f972cf1dfa4b8b596"
  end

  resource "uiop" do
    url "http://beta.quicklisp.org/archive/uiop/2019-05-21/uiop-3.3.3.tgz"
    sha256 "f5a978849233b3e02c8f70d2373c53f74b13c815a355ca074d21855f255e09e5"
  end

  def install
    resources.each do |resource|
      resource.stage buildpath/"lib"/resource.name
    end

    ENV["CL_SOURCE_REGISTRY"] = "#{buildpath}/lib//:#{buildpath}//"
    ENV["ASDF_OUTPUT_TRANSLATIONS"] = "/:/"

    system "sbcl", "--eval", "(require :asdf)", "--eval", "(asdf:load-system :deploy)", "--eval", "(handler-case (progn (setf deploy:*status-output* nil) (asdf:make :lake)) (error () (uiop:quit 1)))"
    bin.install Dir["bin/*"]
  end
end
