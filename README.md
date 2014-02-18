tumbleweed
==========

This is an IRC bot I made to practice writing one.

It waits in the channels you tell it to.  When enough time has passed without
any activity, tumbleweed rolls by, reviving the channel from its death.

Customization
-------------

Near the top of the source code are some haskell definitions.  Change them to
customize tumbleweed.

server: The IRC server to connect to.
port: The port to connect on.
home: The channel tumbleweed will first join.
nick: The nick tumbleweed will claim.
waittime: The time in seconds tumbleweed will wait before rolling by.

Known bugs
----------

If you tell tumbleweed to join a channel but use the wrong case when
specifying the channel name, tumbleweed will join the channel and store the
channel name with the wrong case.  When people talk in the channel, it thinks
they are talking in a separate channel, resulting in tumbleweed rolling by
even when much activity has been present.
