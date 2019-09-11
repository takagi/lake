class Lake < Formula
  desc "Lake is a GNU make like build utility in Common Lisp."
  homepage "https://github.com/svetlyak40wt/lake"
  url "https://github.com/svetlyak40wt/lake/archive/v0.1.2.tar.gz"
  sha256 "656f12b27a3cbff6867c8dfd36d3173be5c9e310395963d17cf0a0e23f3044fa"
  head NIL

  depends_on "sbcl"

  resource "alexandria" do
    url "http://beta.quicklisp.org/archive/alexandria/2019-07-10/alexandria-20190710-git.tgz"
    sha256 "e0642bd6f8af8eb71e3359b45e11a135fa3c9a511492bc9dbbcd10ec7d694704"
  end

  resource "babel" do
    url "http://beta.quicklisp.org/archive/babel/2017-12-27/babel-20171227-git.tgz"
    sha256 "2e0b1e1513d2cf61f23f38f4d2b5fec23efecf88cb72b68aff7d07559334de98"
  end

  resource "bordeaux-threads" do
    url "http://beta.quicklisp.org/archive/bordeaux-threads/2018-07-11/bordeaux-threads-v0.8.6.tgz"
    sha256 "3ee42f65c46801d9277f37ce2253531164c40ead7fe7b255344f80ef574b6be0"
  end

  resource "cffi" do
    url "http://beta.quicklisp.org/archive/cffi/2019-07-10/cffi_0.20.1.tgz"
    sha256 "6a427cc08f0418900bae8a76a690bb1c51fd61caf7efcb677d31701e0ce3ec5e"
  end

  resource "cl-interpol" do
    url "http://beta.quicklisp.org/archive/cl-interpol/2018-07-11/cl-interpol-20180711-git.tgz"
    sha256 "196895b193f955488e51e2f69c2afca8adb9beeca0e2cdfc80c9a7c866a908e9"
  end

  resource "cl-ppcre" do
    url "http://beta.quicklisp.org/archive/cl-ppcre/2019-05-21/cl-ppcre-20190521-git.tgz"
    sha256 "1d4b08ea962612ba92cec7c6f5bb0b8e82efddb91affa0007ada3a95dc66d25c"
  end

  resource "cl-syntax" do
    url "http://beta.quicklisp.org/archive/cl-syntax/2015-04-07/cl-syntax-20150407-git.tgz"
    sha256 "166d32aaf0ed2a218926a1b757abb5c0edbac6fa493f5cba1a89501ce151e9df"
  end

  resource "cl-unicode" do
    url "http://beta.quicklisp.org/archive/cl-unicode/2019-05-21/cl-unicode-20190521-git.tgz"
    sha256 "ecd90df05f53cec0a33eb504b9d3af0356832ad84cc0ddb3d5dc0dbb70f6405c"
  end

  resource "deploy" do
    url "http://beta.quicklisp.org/archive/deploy/2019-05-21/deploy-20190521-git.tgz"
    sha256 "0ccf94ce9826488c2dea584ccc49d8143c5b0485135c54df39f7490cf0cf3d8b"
  end

  resource "documentation-utils" do
    url "http://beta.quicklisp.org/archive/documentation-utils/2019-07-10/documentation-utils-20190710-git.tgz"
    sha256 "433e4ee61d533797a5426310bd9a49de944d747a0532f3f49552cc72b8467fd8"
  end

  resource "flexi-streams" do
    url "http://beta.quicklisp.org/archive/flexi-streams/2019-01-07/flexi-streams-20190107-git.tgz"
    sha256 "259a64ec4f19da7abf64296864a4019daf53c330d1dc9945cefb377df59e13bb"
  end

  resource "lparallel" do
    url "http://beta.quicklisp.org/archive/lparallel/2016-08-25/lparallel-20160825-git.tgz"
    sha256 "213bc89e6bbabe07fc3bcb21be1021b31f6f2ab1b7a2abb358a01ab9bee69c73"
  end

  resource "named-readtables" do
    url "http://beta.quicklisp.org/archive/named-readtables/2018-01-31/named-readtables-20180131-git.tgz"
    sha256 "e5bdcc3f0ef9265785baebbfd5f1c8f41f7a12e8b5dfab8cafa69683457d1eba"
  end

  resource "split-sequence" do
    url "http://beta.quicklisp.org/archive/split-sequence/2019-05-21/split-sequence-v2.0.0.tgz"
    sha256 "6aa973591b2ba75a07774638f4702cdf329c2aa668e3f7e9866a06fab9ae9525"
  end

  resource "trivial-features" do
    url "http://beta.quicklisp.org/archive/trivial-features/2019-07-10/trivial-features-20190710-git.tgz"
    sha256 "be8d97a31a383e1d3c69b87766f3c318f946519d2dc0f80df1fc221921dc2212"
  end

  resource "trivial-gray-streams" do
    url "http://beta.quicklisp.org/archive/trivial-gray-streams/2018-10-18/trivial-gray-streams-20181018-git.tgz"
    sha256 "3b921381df112515661c174fafa04adf11cf4620ebd7e2cc1d7bfd548fab2d28"
  end

  resource "trivial-indent" do
    url "http://beta.quicklisp.org/archive/trivial-indent/2019-07-10/trivial-indent-20190710-git.tgz"
    sha256 "9d79c506d5c12a3916e7d646f355b7bba2a342cf97974203e33b06c7902c4303"
  end

  resource "trivial-types" do
    url "http://beta.quicklisp.org/archive/trivial-types/2012-04-07/trivial-types-20120407-git.tgz"
    sha256 "bdaf0e7174a1be45c8b13854361dcb5967fcb90ab37eae3913178bb8d7727478"
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

    system "sbcl", "--eval", "(require :asdf)", "--eval", "(asdf:load-system :deploy)", "--eval", "(handler-case ((setf deploy:*status-output* nil) (asdf:make :lake)) (error () (uiop:quit 1)))"
    bin.install Dir["src/bin/*"]
  end
end
