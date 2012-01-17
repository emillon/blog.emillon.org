---
title: ZSH suffix aliases
author: Etienne Millon
tags: linux, zsh
---

I recently changed my login shell to use [zsh] instead of the venerable [bash].
I am still wondering why I didn't make the change earlier. Zsh's infamous
slowness almost not perceptible, at least with the default configuration.

One cool feature present in zsh is the notion of *suffix alias* (described in
[zshbuiltins(1)]). Quick example :

    $ alias -s pdf=evince
    $ filename.pdf

… will open filename.pdf under evince, as if `evince filename.pdf` had been
typed. Handy !

But it is not restricted to files : the command is executed whenever the command
line matches a suffix alias. So, for example you can define :

    alias -s git='git clone'

… so that everytime you paste a URL ending in `git`, say
`git://git.debian.org/git/aptitude/aptitude.git`, it will be `git-clone`d.

[zsh]:            http://www.zsh.org/
[bash]:           http://www.gnu.org/software/bash/
[zshbuiltins(1)]: http://linux.die.net/man/1/zshbuiltins
