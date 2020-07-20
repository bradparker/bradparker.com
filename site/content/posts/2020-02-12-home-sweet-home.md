---
title: Home sweet home
tags: development
description: |
  ```
  brad@brad
  ~ » nix-shell -p figlet
  brad@brad [nix:shell]
  ~ » figlet -f smscript nifty

        o |\_|_
  /|/|  | |/ |  |  |
   | |_/|/|_/|_/ \/|/
          |)      (|
  ```

  If you're like me, you develop on both macOS and Linux and are very much a creature of habit, then this may be of interest. It's a description of how I arrived at my current approach for maintaining a base development environment across the two platforms.
---

The operations practice of defining [_infrastructure as code_](https://en.wikipedia.org/wiki/Infrastructure_as_code) has been popular for a while, and it’s for good reason. Lot's of strange and unexpected things can happen to our **very important** web-servers, so being able to quickly build another one in a (close to) identical state can be very handy. Sadly, strange and unexpected things don't just happen to web-servers, they can happen our development machines too. And, heck, they're also pretty important.

Like infrastructure, can we define some of the state of our laptops using code? I'm not sure, but I think so, at least I'll explain what I've tried.

## Bootstrapping scripts

First, I used a [lightly modified fork](https://github.com/bradparker/dotfiles/tree/174c7e07dfa137152fdd941fe187635bb30994f6) of [Ryan Bates' dot files](https://github.com/ryanb/dotfiles) which bootstraps itself using a lovely [rake task](https://github.com/ryanb/dotfiles/blob/ca4d95179b62ceb1a760a2922953edd01d75c382/Rakefile#L5).

This went pretty well. I didn't have to bootstrap new machines too often, I didn't really have many preferences regarding command line tools, and I only had so many technology stacks to be setup for. 

The trouble came when I wanted to _change things_. It wasn't always clear to me that the changes I made would be applied properly when I ran the script on a fresh machine. It always seemed that I missed something and had to do some manual work in addition to the rake task.

The only part that I found to be very predicable were the files that were symlinked into my home directory. So after a while I cut the repository down to [nothing but those](https://github.com/bradparker/dotfiles/tree/28b4dcf7e27d61badef3a29f97d8faef6cc66d0f).

## Staying fresh

A colleague introduced a few us at Everydayhero to [Fresh](https://freshshell.com/). It's great and with it I _just started_ to feel that my development machine had become easily recoverable. I could run `fresh` on a minimally setup Macbook as well as on one I'd been working away on for months and the results were, more or less, the same.

Even though I still didn't do all that much with Fresh that couldn't be achieved with symlinking I did [begin to see the potential](https://github.com/bradparker/dotfiles/blob/663a66ca5959edb20284b487f8580475ec37c822/freshrc#L32-L35).

Despite this I was still looking for something more complete. Fresh managed a lot, but it didn't manage _everything_. The plurality of _thing managers_ I was using became apparent when I finally [documented](https://github.com/bradparker/dotfiles/blob/9e990d99d4bb6b85a2d847d5f47131d974add526/README.md) how I'd been setting everything up.

## Home manager

I'd been using [Nix](https://nixos.org/nix/) as a [Homebrew](https://brew.sh/) replacement for a while before finding [Home manager](https://github.com/rycee/home-manager). It's given me the ability to consolidate almost everything I need to setup and update my development machines into [one command](https://github.com/bradparker/dotfiles/blob/master/README.md#0-optional-run-the-install-script).

Apart from simplifying the process of setting up a new machine, Home manager has also allowed to me to manage more _stuff_ more predictably.

### Utilities

There are a few non-default command line utilities I find myself always installing. Before Home manager if I wanted to setup a new machine I'd usually be moving lists of currently installed dependencies around.

```
brad@old $ brew list > deps.txt

brad@new $ xargs brew install < deps.txt
```

I'd moved to Nix before becoming aware of [Homebrew Bundle's](https://github.com/Homebrew/homebrew-bundle) Brewfile, which looks like a great solution. Just not quite as holistic as I've found Nix / Home manager to be and with it my [list of utilities](https://github.com/bradparker/dotfiles/blob/b721261ea5b1aae448c45e63d99069d79571ca75/nixpkgs/home.nix#L86-L122) is easy to manage.

### Vim

Before Home manager I'd used a few of the popular Vim plugin managers ([Pathogen](https://github.com/tpope/vim-pathogen), [Vundle](https://github.com/VundleVim/Vundle.vim) and [Plug](https://github.com/junegunn/vim-plug)). They all worked well for me, but with my [current setup](https://github.com/bradparker/dotfiles/blob/b721261ea5b1aae448c45e63d99069d79571ca75/nixpkgs/home.nix#L3-L32) I get a good plugin manager for Vim without an extra tool or installation step.

### Tmux

I've only ever used two Tmux plugins. Initially I installed them with [TPM](https://github.com/tmux-plugins/tpm) but now I install them with [Home manager](https://github.com/bradparker/dotfiles/blob/b721261ea5b1aae448c45e63d99069d79571ca75/nixpkgs/home.nix#L40-L48) (Tmux sensible gets included by default).

### Bash

Sometimes it's nice to string utilities together into [useful aliases](https://github.com/bradparker/dotfiles/blob/master/nixpkgs/programs/bash/fuzzy-cd.sh), or try to bring a [little more consistency](https://github.com/bradparker/dotfiles/blob/master/nixpkgs/programs/bash/clipboard.sh) to working across both macOS and Linux. Sadly, those niceties can get out of hand, so I've found it useful to factor them out into [bite-sized chunks](https://github.com/bradparker/dotfiles/blob/b721261ea5b1aae448c45e63d99069d79571ca75/nixpkgs/home.nix#L63-L84).

### Git

I get _a lot_ of use from [Git bash completions](https://github.com/bradparker/dotfiles/blob/master/nixpkgs/home.nix#L57-L59) and [a few handy aliases](https://github.com/bradparker/dotfiles/blob/master/nixpkgs/programs/git/gitconfig#L4-L12).

***

I get all of that, and a few other things, installed and configured with one tool. What's more: 

* It's repeatable, if my environment bootstraps once I'm confident it'll do so again. 
* It's idempotent, I can keep running `home-manager switch` as often as I like without issues.
* It works, more or less, the same on both my work machines running macOS and my home laptop running [PureOS](https://www.pureos.net/), a Debian fork. Because it's so repeatable I'm able to set up a [build pipeline](https://travis-ci.org/bradparker/dotfiles) which ensures that every update I make will install happily on both platforms. I accept that this is perhaps taking things too far.

Now that I've said that, and now that I'm pretty comfortable with the approach, I expect I'll push it a little further. Why not?
