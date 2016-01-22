# GearBot #

GearBot is a simple irc bot that posts deals to the climbing irc channel, #climbing, on snoonet.
The bot is written in Haskell.

## Contributing ##

I know this code is not in a great state. My Haskell knowledge is still growing.
If you have any comments or suggestion I would love to hear them.
Want to submit a PR? I'd be forever grateful.

## Future Thoughts ##
Some things I want to work towards implementing

### Prevent bumps from being posted ###

Since the bot is only seeded with the initial front page posts, a post that is bumped will be triggered like a new post.
The frequency of this happening decreases the longer that bot run.

Possibly prevent this checking number of post in the thread?

### Alert on new post vs title change ###

The bot issues an alert when there is a new title on the front-page. This happens either when a new post happens, or an existing post has had its title changes.
It would be nice to differentiate between these.

### General code cleanup and improvements ###

I need to learn these monad things and write good code. Any PRs will be gladdy accepted. I know the code is ugly.

### Increase Modularity - More Site to Scrape ###
Make each site a module, have the ability to scrape multiple sites at various intervals.

### Configure via environmental variables ###
Pass irc channel, server, etc. either through environmental variables or command line args
