webdrv - WebDriver implementation in Erlang
===========================================

This package contains an implementation of the WebDriver API in Erlang. The WebDriver API
provides a platform- and language-neutral interface that allows programs to control the
behavior and inspect the state of a web browser. It is mainly intended for automated
tests, but can also be used to control a browser for other purposes.

The WebDriver API is accessed via http-requests, data is serialized using JSON.

This package contains two levels; one purerly functional level, where separate WebDriver
commands can be made, and a gen_server based level, wrapping the concept of a WebDriver
session (i.e. an interaction with a browser instance).

The WebDriver API is currently being standardized by W3C (the latest working draft when
writing this is from May 30th 2013), however the main implementors of the server-side of
WebDriver (like Selenium and ChromeDriver) still follow the protocol as described by
Selenium --
[The WebDriver Wire Protocol](https://code.google.com/p/selenium/wiki/JsonWireProtocol).

This implementation spans most of what is covered by the Wire Protocol, with the exception
of the Touch interface, Geographical location and local storage. Extending with these
things should be straightforward but we have not seen a usage of this functionality yet. 

The package also contains tests, in the form of a [QuickCheck](http://quviq.com/) model,
testing both the session wrapper and the Wire Protocol. Page navigation, window control
and element location is well tested, while page element interaction and cookies remains as
TODO.

Building and Installing
-----------------------

There is a simplistic `Makefile` included in the repository. The command:

    make all

compiles all sources to BEAM's, and builds the documentation. `make all` is short for
`make compile` and `make doc`. The command:

    make test

compiles all tests (to `./ebin`). The command:

    make release

creates a ready-to-be-installed directory in `./rel/webdrv-VERSION`. There is no automagic
installation, if you want the library permanently in your code path; copy the generated
directory to your `$ERLANG/lib/`. Finally, the command:

    make clean

removes all(?) generated files from the project.

Contributing
------------

Please don't hesitate to improve and/or extend the implementation or the tests, there is
plenty of room for improvement. The easiest way to contribute is:

1. Fork it.
2. Create a branch (`git checkout -b my_webdriver`)
3. Commit your changes (`git commit -am "Added new cool feature X"`)
4. Push to the branch (`git push origin my_webdriver`)
5. Open a [Pull Request][1]

[1]: http://github.com/Quviq/webdrv/pulls
