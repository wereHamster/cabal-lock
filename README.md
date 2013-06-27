Replace Setup.hs with the file from this repository. Upon the first run of
`runhaskell Setup.hs configure --user`, the file will write down all
dependencies (even the transitive ones) into a file. If you run configure
again and that file exists, it will be used to override version of packages
that need to be installed.

Read more about bundler, Gemfile.lock and the rationale behind it at
[http://gembundler.com/rationale.html].
