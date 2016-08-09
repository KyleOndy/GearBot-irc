# NO LONGER MAINTAINED

I have moved my development efforts to [SuperMunter](https://github.com/SuperMunter/SuperMunter)

# GearBot #
[![Build Status](https://travis-ci.org/KyleOndy/GearBot-irc.svg?branch=master)](https://travis-ci.org/KyleOndy/GearBot-irc)

GearBot is a simple irc bot that posts deals to the climbingdeals irc channel, #climbingdeals, on snoonet.
The bot is written in Haskell.

## Contributing ##

I know this code is not in a great state. My Haskell knowledge is still growing.
If you have any comments or suggestion I would love to hear them.
Want to submit a PR? I'd be forever grateful.

## Fixes Needed ##
* Unicode support? non ascii? chracter crashed the bot.
* **ADD TESTNG**

## Future Thoughts ##
Some things I want to work towards implementing

### #climbing
Let gearbot sit in #climbing. Don't brodcast everything, keep that in #climbinggear.
However, have some command to show latest post, for disscossion purposes.

    someuser  > gearbot latest
    gearbot   > <gear link>
    someuser  > ^ Free Cams!

### Prevent bumps from being posted ###
Possibly prevent this checking number of post in the thread?

### Alert on new post vs title change ###

The bot issues an alert when there is a new title on the front-page. This happens either when a new post happens, or an existing post has had its title changes.
It would be nice to differentiate between these.

### General code cleanup and improvements ###

I need to learn these monad things and write good code. Any PRs will be gladdy accepted. I know the code is ugly.

### Increase Modularity - More Site to Scrape ###
Make each site a module, have the ability to scrape multiple sites at various intervals.

### Configure via environmental variables ###
Pass irc channel, server, etc. either through environmental variables or command line argso
Log level / verbosity of STDOUT

### Recieve notifications via socket ###
Run scrappers as a service. Push to irc bot, pushbulet, etc
