filestore
=====

An OTP application

Build
-----

    $ rebar3 compile

Summary
-------

The file storage is implemented using DETS, there is one table for holding metadata about the chunk and another
for holding the actual chunk data. I chose this method because I thought it'd be the quickest way to implement - 
if I was creating a production system I would use a similar approach, but store files on disk and point to the file
paths in DETS.

* The REST route for creating a file doesn't work, it requires some fiddling around with cowboy
and multipart HTTP requests - I haven't done this before and didn't have time to work it out
* The chunk REST route has errors display the data as `jsone` can't encode the binary chunk data
to return a response back via cowboy.

* Distributed tests work, please see store_dist_SUITE
* There are some REST api tests for the metadata API which is complete
* Dialyer passes - tested on OTP 20

I made some tradeoffs in my design which I wouldn't normally do in a production system. For example, 
i've made heavy use of rpc calls where in a production environment I would be reluctant to do so, preferring
to use gen_server:call and friends directly.

If you look through my early commits I originally had a pool of TCP connections to write the chunk data via.
This is the route i'd go in a production environment to avoid the pitfalls of RPC and/or passing large binar messages
via message passing between nodes. The TCP pool was going to work by having a node contact the target node, where the target assigned a port
for the source node to connect to and send that back for the source to dynamically create its connection.

I also wanted to have my file readers be state machines, but in the interest of simplicity decided to use `proc_lib` processes.

There is more to build on in this small example, it would have been nice to be able to do things such as:
* Implement a read / write quorum where R nodes have to respond to read, W to write
* Implement copying of data across multiple nodes in the cluster - your "N" value in Riak terminology. Where a piece of data
is copied across the cluster N times for redundancy.
* Implement a web client to easily view files
* Implement replication and hand-offs between nodes - if a node crashes we can bring it back up to a good state and hand-over any new 
data that needs to be stored.
* File checksum validation. I've included the checksum of the chunk data in my metadata DETS table in `store`
to show it's possible to store more information on the chunk data - next step would be to use checksums to verify writes
are being performed with the correct data on the remote node and abort if not.

If there is anything that you wish to discuss about my design, or if you'd like to talk about improvements with me
then please get in touch. Most of my ideas are currently shaped off of how Riak works as that is what I am working on
full-time as of this moment.

To test
--------

    $ make rel
    $ rebar3 ct

Due to time constraints there are errors in:
* store_dist_SUITE - testing read doesn't work, wasn't able to debug why in time to submit. However read
works fine from the shell (running in local cluster) as below.


Test functions in local cluster
--------------------------------
    $ make rel
    $ ./priv/start_cluster.sh <Number of nodes>
    $ ./priv/attach.sh <Node index to connect to>

Configuration
---------------
To edit the configuration please see `priv/vars.config` and `priv/sys.config`, e.g to edit the number of nodes and
chunk size. 


Retrospective
-------------

A lot of the difficulty came by trying to implement a full system as I would write for a production environent to start off with,
so I made the decision to massively simplify my design in order to get a MVP out.

More difficult was actually setting a new product from scratch using Rebar3 - I forgot how painful that is, you take it
for granted once you're already working on established products.