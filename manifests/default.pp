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
