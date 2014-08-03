# Tigyog

The idea of Togyog is that *the entire state of your project is managed in your
git repository*. In particular this means:

* Your documentation lives next to your code in your repository.
* Your tasks, issues, bugs, and feature requests are tracked right there next to
  your code in your repository.
* Your merge requests and code reviews are tracked in your repository.
* Your access control policies are tracked in the repository.
* Your CI configuration is stored in your repository.
* Your build results are recorded in your repository.
* Your external contributors can contribute directly to subsets of your
  repository.

You might summarize this as Atlassian + Git-as-database.

The expectation is that using the repository that you already manage as the
backing database for all your development management, instead of several
centralized ad-hoc databases, will make things both simpler and more powerful.

Tigyog will succeed to the extent that this is true, possible, and feasible.


# Installation

Installation has to be *really fucking simple.*

As a user, I have my git repository on a server, and I want to run one command
to get up-and-running. I have `bash` and `curl` available, since they're
universal. I have `git` available, since I have a git repository to hand. I do
*not* have any Haskell stuff, but I can run statically linked binaries compiled
from Haskell. I have a decent machine and files < 100MB are trivial to download
and persist.

Following the philosophy, Tigyog also manages its *own* state in the repository,
too:

* Repository should track desired version of Tigyog.
* Repository should contain config options for Tigyog.
* Repository should *not* contain Tigyog executables. If placed in the
  directory, they should be `.gitignore`d. There must exist a function from
  Tigyog version strings to Tigyog binaries which can be executed on a machine
  with `bash`, `curl`, and `git`.

Run something like this in the root of the git repository:

    curl -sS https://raw.githubusercontent.com/jameshfisher/tigyog/master/tigyog_installer.bash | bash

End result is:

* a `/tigyog` or `/tigyog.exe` binary is placed in the root of the repository
* this file is added to `/.gitignore` on `master` branch, if not already ignored
* the `tigyog_installer.bash` file is added on `master` branch. This file tracks
  the desired version with a variable that defaults to `'master'`. Running the
  script ensures Tigyog is up-to-date with that desired version.

The `tigyog_installer.bash` file is extremely minimal; not much more than

    #!/bin/bash
    TIGYOG_VERSION=master
    curl -o tigyog https://path/to/tigyog/${TIGYOG_VERSION}
    echo "tigyog" >> .gitignore
    echo "Please commit the results."

