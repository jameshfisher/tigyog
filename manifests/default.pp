package { "haskell-platform":
  ensure => present,
  provider => 'apt'
}

file { "/etc/profile.d/cabal-bin.sh":
  ensure => present,
  content =>
'#!/bin/sh
export PATH=${PATH}:~/.cabal/bin
',
}

package { "git":
  ensure  => present,
  provider => 'apt'
}

# This silences a warning "WARNING! Your environment specifies an invalid locale."
# when SSHing to the machine.
package { "language-pack-en":
  ensure  => present,
  provider => 'apt'
}

# Required for gitlib-libgit2
package { "libssl-dev":
  ensure  => present,
  provider => 'apt'
}

# Required for gitlib-libgit2
package { "libicu-dev":
  ensure  => present,
  provider => 'apt'
}

exec { "/usr/bin/cabal update":
  require => Package["haskell-platform"],
  user => "vagrant",
  environment => ["HOME=/home/vagrant"]
}

package { "ruby":
  ensure  => present,
  provider => 'apt'
}

package { "ruby-dev":
  ensure => present,
  provider => 'apt'
}

package { "fpm":
  require => [ Package["ruby"], Package["ruby-dev"] ],
  ensure => present,
  provider => 'gem'
}

# Required to build rpms with fpm
package { "rpm":
  ensure  => present,
  provider => 'apt'
}

exec { "/usr/bin/cabal install shake":
  require => Package["haskell-platform"],
  user => "vagrant",
  environment => ["HOME=/home/vagrant"]
}

exec { "/usr/bin/cabal install elm":
  require => Package["haskell-platform"],
  user => "vagrant",
  environment => ["HOME=/home/vagrant"]
}

package { "libncurses5-dev":
  ensure  => present,
  provider => 'apt'
}

exec { "/usr/bin/cabal install elm-server":
  require => [Package["haskell-platform"], Package["libncurses5-dev"]]
  user => "vagrant",
  environment => ["HOME=/home/vagrant"]
}

exec { "/usr/bin/cabal install elm-repl":
  require => Package["haskell-platform"],
  user => "vagrant",
  environment => ["HOME=/home/vagrant"]
}

exec { "/usr/bin/cabal install elm-get":
  require => Package["haskell-platform"],
  user => "vagrant",
  environment => ["HOME=/home/vagrant"]
}
