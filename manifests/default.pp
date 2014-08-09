package { "haskell-platform":
  ensure  => present
}

package { "git":
  ensure  => present
}

# This silences a warning "WARNING! Your environment specifies an invalid locale."
# when SSHing to the machine.
package { "language-pack-en":
  ensure  => present
}

# Required for gitlib-libgit2
package { "libssl-dev":
  ensure  => present
}

# Required for gitlib-libgit2
package { "libicu-dev":
  ensure  => present
}

exec { "/usr/bin/cabal update":
  require => Package["haskell-platform"],
  user => "vagrant",
  environment => ["HOME=/home/vagrant"]
}
