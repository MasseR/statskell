Abstract
========

statskell is a statistics collection daemon. It came to be after reading
[http://codeascraft.etsy.com/2011/02/15/measure-anything-measure-everything/](Measure
anything, measure everything) article. The purpose is to have a simple
interface for counting 'stuff'. For example you can record failed and
successful login attempts of your site. If there is a weird spike in the failed
login attempts, you can probably assume that there has been some sort of
hacking attempt. Another use-case is for example calculating how many emails
you receive within a timeperiod. I have a fetchmail/procmail setup, and I can
increase the counter from within procmail when any email arrive, statskell then
flushes the received counter to rrdtool.

statskell works by receiving udp messages, aggregating them and then flushing
them out after a 10 second period. It flushes the aggregated stats into a
[http://oss.oetiker.ch/rrdtool/](rrd) database. I chose rrdtool as the backend
because it handles graphing and its database is constant size. If you want to
read more about it, go to [http://oss.oetiker.ch/rrdtool/](rrdtool website).

Use
===

statskell is in alpha-stage, but it should be stable enough. All you need to is
to [http://oss.oetiker.ch/rrdtool/doc/rrdcreate.en.html](create the rrdtool
database) and start the server. It listens on port 4242 udp packets and writes
the results into `stats.rrd` file into the directory where the server has been
started.

Protocol
--------

The protocol is a simple 4 column protocol separated by pipe character ('|').

    bucket_name|value|type|consolidation

Possible values for the type field:

- absolute Absolute value. See rrdtool documentation for what it means
- gauge Gauged value. See rrdtool documentation for what it means

Possible values for the consolidation field:

- sum Sum together all the values. For example if you get results faster then the flush-delay, the values are added together
- max Select the maximum of all the values
- min Select the minimum of all the values
- average Calculate the average of all the values

Todo
====

- Configuration
    - Configurable port
    - Configurable database path
- Better error reporting
    - Error reporting happens concurrently, mixing the letters.
