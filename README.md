# Perftools.erl

Capture trace information using Erlang's built-in trace
functionality. Convert trace data to a profile according to [Google's
CPU Profiler binary data
format](http://google-perftools.googlecode.com/svn/trunk/doc/cpuprofile-fileformat.html).

Render profile data using
[pprof](http://google-perftools.googlecode.com/svn/trunk/doc/cpuprofile.html#pprof).


## Usage

Get the code:

    git clone git://github.com/dreverri/perftools.erl.git

Build the CLI tool:

    make

Run a trace (example assumes a Riak node is running):

    ./perftools trace riak.trace \
    -name test@127.0.0.1 \
    -setcookie riak \
    -node riak@127.0.0.1 \
    -function riak_core_vnode_master

Profile the trace data:

     ./perftools profile riak.trace riak.prof

Analyze the data:

     ./bin/pprof riak.prof --gif > riak.gif
     open riak.gif


## Trace

## Profile

## Analyze

## Todo

Documentation
