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

  If you're like me, you develop on both macOS and Linux and are very much a creature of habit, then this may be of interest. It's a description of how I maintain my base development environment (my "dot files") across the two platforms.
---

The operations practice of defining [_infrastructure as code_](https://en.wikipedia.org/wiki/Infrastructure_as_code) has been popular for a while, and it’s for good reason. Lot's of strange and unexpected things can happen to our **very important** web-servers, so being able to quickly build another one in a (close to) identical state can be very handy. Sadly, strange and unexpected things don't just happen to web-servers, they can happen our development machines too. And, heck, they're also pretty important.

Like infrastructure, can we define some of the state of our laptops using code? I'm not sure, but I think so, at the least I'll explain what I've tried.

## Bootstrapping scripts

First, I used a [lightly modified fork](https://github.com/bradparker/dotfiles/tree/174c7e07dfa137152fdd941fe187635bb30994f6) of [Ryan Bates' dot files](https://github.com/ryanb/dotfiles) which bootstraps itself using a lovely [rake task](https://github.com/ryanb/dotfiles/blob/ca4d95179b62ceb1a760a2922953edd01d75c382/Rakefile#L5).

This went great. I didn't have to bootstrap new machines too often, I didn't really have many preferences regarding command line tools, and I only had so many technology stacks to be setup for. 

The trouble came when I wanted to _change things_. It wasn't always clear to me that the changes I made would be applied properly when I ran the script on a fresh machine. It always seemed that I missed something and had to do some manual work in addition to the rake task.

The only part that I found to be very predicable were the files that were symlinked into my home directory. So after a while I cut the repository down to [nothing but those](https://github.com/bradparker/dotfiles/tree/28b4dcf7e27d61badef3a29f97d8faef6cc66d0f).

## Staying fresh

Nick introduced a few us at Everydayhero to [Fresh](https://freshshell.com/). It's great and with it I _just started_ to feel that my development machine had become easily recoverable. I could run `fresh` on a minimally setup Macbook as well as on one I'd been working away on for months and the results were, more or less, the same.

Even though I still didn't do all that much with Fresh that couldn't be achieved with symlinking I did [begin to see the potential](https://github.com/bradparker/dotfiles/blob/663a66ca5959edb20284b487f8580475ec37c822/freshrc#L32-L35).

Despite this I was still looking for something more complete. Fresh managed a lot, but it didn't manage _everything_. The plurality of _thing managers_ I was using became apparent when I finally [documented](https://github.com/bradparker/dotfiles/blob/9e990d99d4bb6b85a2d847d5f47131d974add526/README.md) how I'd been setting everything up.

## Home manager

I'd been using [Nix](nixos.org/nix/) as a [Homebrew](https://brew.sh/) replacement for a while before finding [Home manager](https://github.com/rycee/home-manager).
