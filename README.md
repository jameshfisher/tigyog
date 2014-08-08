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


# Bug tracker < Wiki

A wiki is an extremely general form of information tracker. It is possible that
specific tools could be subsumed by the general form of a wiki; if so, this is a
nice generalization and simplification.

An issue is just a page in the wiki filed under "issues". The page title is the
issue title. The page contents are the issue description. Page attachments are
issue attachments. Issue comments are subsumed by the issue description. Allow
pages to have key- value data attached; nearly all issue data is just key-value
data.

Cross-application linking (e.g. "link to an issue from a wiki page" and vice
versa) becomes just normal wiki linking.

A page identifier could just be the filepath in the repository.

Wiki pages can link out to code by filepath references. E.g. a wiki page
"overview" can say, `See the [/Makefile] for implementation.` Code can link out
to wiki pages, e.g. comments can say `/* FIXME see [wiki:/issues/does-not-build-on-x64] */`. Links could include line/character references and so on.

If you're a literate programming kinda guy, you could see this as breaking down
the distinction between wiki and code. If you prefer to maintain that
distinction, that's cool too.

Notice that, since a link on one commit refers to a file in the same commit,
it's harder for links to get broken, and easier to fix them when they do. This
fixes these issues:

* Have you ever found an issue number in an old commit, only to discover that
  the company doesn't use that issue tracker any more, and no one has any idea
  what happened to it? That's a broken link, sucker. If they had been using
  Tigyog, even if you don't use it any more, you could just look at the issue
  page in the same commit.
* Have you ever found a reference to some code somewhere, only to find that the
  code has changed? People do this in issue comments all the time. Here's [a
  blog post about it](http://andrew.yurisich.com/work/2014/07/16/dont-link-that-
  line-number/). If you were using Tigyog, that link wouldn't break.

Another cool thing is that, since Tigyog knows about Git, those links can, too.
Git provides a generalization of this kind of link: something which resolves to
an object in the Git database. The links above get resolved by saying: "find the
file at the path of the tree given by the commit on which this link is present."
But we can generalize this in all kinds of ways. A link `[a3f12bc:/foo/bar]`
says "find the file at path `/foo/bar` on the treeish `a3f12bc`." So, if you
want, you can easily link back in history. Or you can link to commits, not
files. And so on.

Imagine if issues in the wiki were called things like `fix/does-not-build-on-x64`.
Then if you create a branch called `fix/does-not-build-on-x64`, Tigyog knows
that the branch is specific to that issue.

Is it possible to get "CI build results" into this paradigm, too? Say, a CI
build corresponds to a wiki page?

You know how specific releases come with a list of known issues? You've seen
that in, say, a `BUGS` file, or in a `man` page? If you're using JIRA or
whatever, then generating that list in your build is non-trivial. But if you're
using Tigyog, it's real easy: the build system just reads the issue files in
your repository, in the same way that it reads code files and config files and
so on.

One conceptual issue here is how to represent transient facts about the "outside
world", external to the repository. One important kind of external fact is "this
issue is assigned to Jack". What does it mean for an issue to be assigned to
Jack on the master branch, but assigned to John on a different branch?


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

