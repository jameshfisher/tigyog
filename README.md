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


# Issues ⊆ Documentation

A wiki is an extremely general form of information tracker. It is possible that
specific tools could be subsumed by the general form of a wiki; if so, this is a
nice generalization and simplification.

An issue is just a page in the wiki filed under "issues". The page title is the
issue title. The page contents are the issue description. Page attachments are
issue attachments. Issue comments are subsumed by the issue description. Allow
pages to have key- value data attached; nearly all issue data is just key-value
data.

This suggests we should think of issues as being *part of the documentation*.
Each open issue is a documented fact that says "sorry, this isn't fixed", or
"sorry, this isn't implemented". You could also notice how project documentation
is usually divided up in the same way as the project's user stories. What is the
difference between a closed issue called `feature/search-bar`, which represents
the fact that a task to implement a search-bar has been completed, and a page in
your documentation called `feature/search-bar`, which represents the fact that
the application features a search-bar and says how to use it? The information
content is the same, isn't it? Can't we interpret "completed feature request
issues" and "documented features" as one and the same thing? And similarly,
can't we interpret "uncompleted bugfix tasks" and "known bugs" as identical?

A failure to recognize this results in poor quality documentation, or *no
documentation*, despite a lot of effort going into writing user stories. Every
been on a project with loads of issues in JIRA, but virtually nothing in
Confluence, or where Confluence is always out of date? I have. If we equate
*writing the issue* and *writing the documentation*, then we suddenly get:

* higher-quality issue descriptions. People put in the effort, because end-users
  will see it, and it's not transient.
* higher-quality documentation. You *can't avoid* writing the documentation,
  because if you don't document it then it won't get implemented. In other
  words, documentation is pre-implementation, not post-implementation, and this
  is enforced by the workflow.
* less duplicated effort.

Some people also put considerable effort into their commit messages: when they
implement a feature, they will put precise documentation in the commit message
of the commit that implements it. This is another "documentation black hole":
end users don't ever read commit messages of the software they use. Instead of
doing the documentation in the commit message, you should do it *in the
documentation*.


# Links!

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


# How to display history and versioning

One simple approach to displaying pages is: take all content from the current
master branch. When the master branch moves, the database is updated. In other
words, the state of the database is the tree of the commit of the master branch.

But this makes Tigyog unaware of history and versioning. The whole point here is
to have documentation and issue tracking which is aware of history (previous
commits) and aware of versioning (branches).

A slightly better approach would be to allow the user to jump around between
treeishes. An input box lets the user enter 'release/1.6', hit return, and get
shown the state of the wiki/issues on the tree of the commit pointed at by the
branch `release/1.6`. Or they can enter '8534a23' and see the state of the wiki
and issues on commit `8534a23`.

Such an approach says: first find the commit you want to look at, then find the
page you want to look at in that commit. But I think most users want to invert
that: they want to first find the page they are interested in, *then* see its
history and versions.

Implicit here is that *pages have an identity that is preserved across commits*.
Let's say this identity is the path to the page, e.g. `issues/fix/does-not-build-on-x64`. Users expect to first point their browser at `/issues/fix/does-not-build-on-x64`,
see a "most current version", and then *optionally* see history/versions.

So an improvement would be that the page at `/issues/fix/does-not-build-on-x64`
defaults to the master branch, but has an input/select box to select
past/different versions.

This is still not very slick, though. I would like to see those differences
*integrated* on the page. If a bug page is marked as `status: fixed` on `master`
and every other branch, but as `status: unfixed` on `release/1.6`, I would like
the UI to show me `status: fixed (unfixed on release/1.6)`. Similarly for other
attributes like the description.

At the moment, we use systems that post to our issue trackers when we merge a
change. For example, we might have a rule that says, "when a merge on master
occurs, if it merges a branch with the name of an issue, post a comment on that
issue to say that it was merged to master". These kind of integration rules are
brittle and complex. Notice how just using Git *as* the issue tracker does this
work for you.


# Users

The system has *users*. This must be integrated with user data in Git. When a
user makes a change to a page in the UI, the commit author is *not* Tigyog, it
is the user. When a developer adds a commit, the work must show up under her
name in Tigyog.

Git identifies users by email address. So must Tigyog. Don't bother with
"username" nonsense.

How do users get authenticated? Tigyog should use some sort of external SSO
system for this. This makes Tigyog simpler and more modular, but also solves an
architectural problem: Tigyog can't store user's credentials (password hashes
etc.), because it only stores things in the repository!

A challenge will be to find an SSO solution that works for all business cases,
e.g. company X wants to use their hideous custom ancient internal user
authentication system.


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

