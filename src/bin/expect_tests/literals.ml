open Cli_expect

let%expect_test _ =
  run_ligo_good ["interpret" ; "(\"edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7\":signature)" ; "--syntax=pascaligo"] ;
  [%expect {| Signature edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7 |}]

let%expect_test _ =
  run_ligo_bad ["interpret" ; "(\"thisisnotasignature\":signature)" ; "--syntax=pascaligo"] ;
  [%expect {|
    ligo: error
    in file "", line 0, characters 0-33
    Badly formatted literal: Signature thisisnotasignature


    If you're not sure how to fix this error, you can do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}]

let%expect_test _ =
  run_ligo_good ["interpret" ; "(\"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav\":key)" ; "--syntax=pascaligo"] ;
  [%expect {| key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav |}]

let%expect_test _ =
  run_ligo_bad ["interpret" ; "(\"thisisnotapublickey\":key)" ; "--syntax=pascaligo"] ;
  [%expect {|
    ligo: error
    in file "", line 0, characters 0-27
    Badly formatted literal: key thisisnotapublickey


    If you're not sure how to fix this error, you can do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |}]
