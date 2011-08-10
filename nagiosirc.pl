#!/usr/bin/perl -w
#
# The contents of this file are subject to the Mozilla Public
# License Version 1.1 (the "License"); you may not use this file
# except in compliance with the License. You may obtain a copy of
# the License at http://www.mozilla.org/MPL/
#
# Software distributed under the License is distributed on an "AS
# IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
# implied. See the License for the specific language governing
# rights and limitations under the License.
#
# The Original Code is the Bugzilla Status Bot
#
# The Initial Developer of the Original Code is David D. Miller.
# Portions developed by David D. Miller are Copyright (C) 2002-3
# David D. Miller.  All Rights Reserved.
#
# Contributor(s): David Miller <justdave@syndicomm.com>
#                 Mark Smith <mark@qq.is>
#

use strict;
use Net::IRC;
use POSIX "sys_wait_h";
use Term::ANSIColor qw/ :constants /;

#
#  Create the IRC and Connection objects
#

my $version = "BZBot v1.3 - Modified for use with Nagios.";
my $irc = new Net::IRC;

#open IPADDR, "</etc/ipaddr";
#my $ipaddr = <IPADDR>;
#chomp ($ipaddr);
#close IPADDR;

# CONFIG: point this where your Nagios configuration files live
my $nagioslog = "/var/log/nagios3/nagios.log";
my $nagioscmd = "/var/lib/nagios3/rw/nagios.cmd";

open NAGIOS, "<$nagioslog" or die "failed to open $nagioslog: $!\n";
seek NAGIOS, 0, 2; # seek to end

my @cmdqueue = ();
my %ignore = ();
my $ACKCT = 0;
my @ACKS;

print "Creating connection to IRC server...\n";

my $conn;
while (!$conn) {
    # CONFIG: you have to tell us where to get on IRC
    $conn = $irc->newconn(Server   => 'irc',
                          Port     => 6667,
                          SSL      => 0,
                          Nick     => 'fidget',
                          Ircname  => 'Nagios alerts',
                          Username => 'nagios')
    or print "Redialing...\n";
    sleep 1;
}

print "Connection created.\n";
$conn->debug(1);
my $laststat = time;
my $identified_to_nickserv = 0;

#
#  Handler subs
#

# What to do when the bot successfully connects.
sub on_connect {
    my $self = shift;

    $identified_to_nickserv = 1;
    print "Joining #status...\n";

    # FIXME: this is broken right now.  when this is re-added, it has to happen
    # before we try to join channels.
    #print "Identifying to NickServ...\n";
    #$self->privmsg('nickserv',"identify xxx");

    # CONFIG: channels you want us to announce to ...
    $self->join("#monitoring");
    $self->join("#status");
}

# Handles some messages you get when you connect
sub on_init {
    my ($self, $event) = @_;
    my (@args) = ($event->args);
    shift (@args);
    
    print "*** @args\n";
}

# What to do when someone leaves a channel the bot is on.
sub on_part {
    my ($self, $event) = @_;
    my ($channel) = ($event->to)[0];

    printf "*** %s has left channel %s\n", $event->nick, $channel;
}

# What to do when someone joins a channel the bot is on.
sub on_join {
    my ($self, $event) = @_;
    my ($channel) = ($event->to)[0];
    my ($nick, $userhost) = ($event->nick, $event->userhost);

    printf "*** %s (%s) has joined channel %s\n",
    $nick, $userhost, $channel;
}

# What to do when we receive a private PRIVMSG.
sub on_msg {
    my ($self, $event) = @_;
    my ($nick) = $event->nick;
    my ($arg) = ($event->args);

    print "*$nick*  ", ($event->args), "\n";
    if ($arg =~ /Go away/i) {       # Tell him to leave, and he does.
        $self->quit("Yow!!");
        exit 0;
    # CONFIG: if you don't change this, anybody can restart your IRC bot at will
    # if they've seen this ...
    } elsif ($arg =~ /restart ya dope/) {
        $self->quit("Regrouping");
        exec $0;
    } else {
        $self->privmsg($nick, "Yo!");
    }
}

# What to do when we receive channel text.
sub on_public {
    my ($self, $event) = @_;
    my @to = $event->to;
    my ($nick, $mynick) = ($event->nick, $self->nick);
    my ($arg) = ($event->args);

    # Note that $event->to() returns a list (or arrayref, in scalar
    # context) of the message's recipients, since there can easily be
    # more than one.

    # FIXME: the logging gets spammy ... do we actually want to log to stdout?
    print "<$nick> $arg\n";

    if ($arg =~ /^$mynick[,: ]/i) {
        if ($arg =~ /^$mynick[,: ]\s*ignore (.+)\s*$/i) {
          $ignore{$1} = 1;
          $self->privmsg([ @to ], "$nick: ok, ignoring notifications about $1");
        } elsif ($arg =~ /^$mynick[,: ]\s*ignore$/i) {
          if (keys %ignore) {
            foreach my $i (keys %ignore) {
              $self->privmsg([ @to ], "$nick: ignoring $i");
            }
            $self->privmsg([ @to ], "$nick: end of list");
          } else {
            $self->privmsg([ @to ], "$nick: I'm not currently ignoring any notifications");
          }
        } elsif ($arg =~ /^$mynick[,: ]\s*unignore (.+)\s*$/i) {
          if (!exists($ignore{$1})) {
            $self->privmsg([ @to ], "$nick: notifications about $1 weren't being ignored anyway.");
          } else {
            delete $ignore{$1};
            $self->privmsg([ @to ], "$nick: ok, notifications about $1 no longer being ignored.");
          }
        } elsif ($arg =~ /^$mynick[,: ]+(?:\s*ack\s*)?(\d+)(?:\s*ack\s*)?[:\s]+([^:]+)\s*$/) {
          open CMDPIPE,">",$nagioscmd;
          my ( $host, $svc ) = @{ $ACKS[$1] || [ undef, undef ] };
          if ( defined $host && defined $svc ) {
              printf CMDPIPE "[%lu] ACKNOWLEDGE_SVC_PROBLEM;%s;%s;1;1;1;%s;%s\n",time(),$host,$svc,$nick,$2;
          } elsif ( defined $host ) {
              printf CMDPIPE "[%lu] ACKNOWLEDGE_HOST_PROBLEM;%s;1;1;1;%s;%s\n",time(),$host,$nick,$2;
          }
          close CMDPIPE;
        } elsif ($arg =~ /^$mynick[,: ]\s*ack ([^:]+):([^:]+)\s*$/) {
          open CMDPIPE,">",$nagioscmd;
          printf CMDPIPE "[%lu] ACKNOWLEDGE_HOST_PROBLEM;%s;1;1;1;%s;%s\n",time(),$1,$nick,$2;
          close CMDPIPE;
        } elsif ($arg =~ /^$mynick[,: ]\s*ack ([^:]+):([^:]+):([^:]+)\s*$/) {
          open CMDPIPE,">",$nagioscmd;
          printf CMDPIPE "[%lu] ACKNOWLEDGE_SVC_PROBLEM;%s;%s;1;1;1;%s;%s\n",time(),$1,$2,$nick,$3;
          close CMDPIPE;
        } elsif ($arg =~ /^$mynick[,: ]\s*unack (\d+)$/) {
          open CMDPIPE,">",$nagioscmd;
          my ( $host, $svc ) = @{ $ACKS[$1] || [ undef, undef ] };
          if ( defined $host && defined $svc ) {
              printf CMDPIPE "[%lu] REMOVE_SVC_ACKNOWLEDGEMENT;%s;%s\n",time(),$host,$svc;
              $self->privmsg([ @to ], "$nick: ok, acknowledgment (if any) for $host:$svc has been removed.");
          } elsif ( defined $host ) {
              printf CMDPIPE "[%lu] REMOVE_HOST_ACKNOWLEDGEMENT;%s\n",time(),$host;
              $self->privmsg([ @to ], "$nick: ok, acknowledgment (if any) for $host has been removed.");
          }
          close CMDPIPE;
        } elsif ($arg =~ /^$mynick[,: ]\s*unack ([^:]+)\s*$/) {
          open CMDPIPE,">",$nagioscmd;
          printf CMDPIPE "[%lu] REMOVE_HOST_ACKNOWLEDGEMENT;%s\n",time(),$1;
          close CMDPIPE;
          $self->privmsg([ @to ], "$nick: ok, acknowledgment (if any) for $1 has been removed.");
        } elsif ($arg =~ /^$mynick[,: ]\s*unack ([^:]+):([^:]+)\s*$/) {
          open CMDPIPE,">",$nagioscmd;
          printf CMDPIPE "[%lu] REMOVE_SVC_ACKNOWLEDGEMENT;%s;%s\n",time(),$1,$2;
          close CMDPIPE;
          $self->privmsg([ @to ], "$nick: ok, acknowledgment (if any) for $1:$2 has been removed.");
        } elsif ($arg =~ /^$mynick[,: ]\s*status (\S.*?)\s*$/) {
          my @lines = ();
          open CMDPIPE,"-|","/usr/local/bin/nag-stat",$1;
          while (<CMDPIPE>) {
            chomp;
            push @lines, $_;
          }
          close CMDPIPE;
          foreach my $line (@lines) {
            $self->privmsg([ @to ], "$nick: $line");
          }
        } elsif ($arg =~ /^$mynick[,: ]\s*help/i) {
          $self->privmsg([ @to ], "$nick: You're right, I need help!");
        } elsif ($arg =~ /(?:hey|hi|hello|yo)(?:\.|,|\!|\s_)/i) {
          $self->privmsg([ @to ], "Yo, $nick!");
        } else {
#          $self->privmsg([ @to ], "Yo!");
        }
    }
}

# Prints the names of people in a channel when we enter.
sub on_names {
    my ($self, $event) = @_;
    my (@list, $channel) = ($event->args);    # eat yer heart out, mjd!

    # splice() only works on real arrays. Sigh.
    ($channel, @list) = splice @list, 2;

    print "Users on $channel: @list\n";
}

sub on_ping {
    my ($self, $event) = @_;
    $self->sl("PONG " . join(' ', ($event->args)));
}

sub on_pong {
    my ($self, $event) = @_;
    # don't really need to do anything, just update the time.
}

# Yells about incoming CTCP PINGs.
sub on_ctcpping {
    my ($self, $event) = @_;
    my $nick = $event->nick;

    $self->ctcp_reply($nick, join (' ', ($event->args)));
    print "*** CTCP PING request from $nick received\n";
}

# Yells about incoming CTCP VERSIONs.
sub on_ctcpversion {
    my ($self, $event) = @_;
    my $nick = $event->nick;

    $self->ctcp_reply($nick, join (' ', ($event->args, $version)));
    print "*** CTCP VERSION request from $nick received\n";
}

# Gives lag results for outgoing PINGs.
sub on_ping_reply {
    my ($self, $event) = @_;
    my ($args) = ($event->args)[1];
    my ($nick) = $event->nick;

    $args = time - $args;
    print "*** CTCP PING reply from $nick: $args sec.\n";
}

# Change our nick if someone stole it.
sub on_nick_taken {
    my ($self) = shift;
    my ($nick,$num) = $self->nick =~ /^(.+)(\d+)?$/;
    $num ||= 0;
    $self->nick($nick . ($num + 1));
}

# Display formatted CTCP ACTIONs.
sub on_action {
    my ($self, $event) = @_;
    my ($nick, @args) = ($event->nick, $event->args);

    print "* $nick @args\n";
}

# Reconnect to the server when we die.
sub on_disconnect {
    my ($self, $event) = @_;

    print "Disconnected from ", $event->from(), " (",
      ($event->args())[0], "). Attempting to reconnect...\n";
    $identified_to_nickserv = 0;
    while (!$self->connected) {
        $self->connect();
        if (!$self->connected) { print "Redialing...\n" }
    }
}

# Look at the topic for a channel you join.
sub on_topic {
}

sub on_notice {
    my ($self, $event) = @_;

    my @args = $event->args();

    # FIXME: the nickserv path is broken ...
    if ((lc($event->nick) eq "nickserv") && ($args[0] =~ "nickname is registered")) {
#        print "Identifying to NickServ...\n";
#        $self->privmsg('nickserv',"identify xxx");
    }

    # FIXME: the nickserv path is broken ...
    if ((lc($event->nick) eq "nickserv") && ($args[0] =~ "You are now identified")) {
        #nickserv identification successful, so let's join channels.
        $identified_to_nickserv = 1;
        print "Joining channels...\n";

        # CONFIG: channels that should be joined go here.
        $self->join("#monitoring");
        $self->join("#status");
    }
}

sub ackable {
    my ( $host, $svc, $state, $msg ) = @_;
    return '' unless $state eq 'WARNING' || $state eq 'CRITICAL' || $state eq 'UNKNOWN' || $state eq 'DOWN';

    my $id = $ACKCT++ % 100;
    $ACKS[$id] = [ $host, $svc, $state, $msg ];
    return sprintf( '[%02d] ', $id );
}

print "Installing handler routines...";

$conn->add_global_handler('ping', \&on_ping);
$conn->add_global_handler('pong', \&on_pong);
$conn->add_handler('cping',     \&on_ctcpping);
$conn->add_handler('crping',    \&on_ping_reply);
$conn->add_handler('cversion',  \&on_ctcpversion);
$conn->add_handler('msg',       \&on_msg);
$conn->add_handler('public',    \&on_public);
$conn->add_handler('caction',   \&on_action);
$conn->add_handler('join',      \&on_join);
$conn->add_handler('part',      \&on_part);
#$conn->add_handler('topic',     \&on_topic);
$conn->add_handler('notopic',   \&on_topic);
$conn->add_handler('notice',    \&on_notice);

$conn->add_global_handler([ 251,252,253,254,302,255 ], \&on_init);
$conn->add_global_handler('disconnect', \&on_disconnect);
$conn->add_global_handler(376, \&on_connect);
$conn->add_global_handler(433, \&on_nick_taken);
$conn->add_global_handler(353, \&on_names);

print " done.\n";

my %C = (
    W => "\x0315",
    B => "\x032",
    G => "\x033",
    R => "\x034",
    O => "\x037",
    C => "\x0311",
    Y => "\x038",
    N => "\x0315",
);

my $state_to_color = {
    OK => $C{G},
    WARNING => $C{Y},
    CRITICAL => $C{R},
    UNKNOWN => $C{C},
};

print "starting...\n";

my %renot; # { "host" or "host:service" => time_last_notification }
while (1) {
    $irc->do_one_loop();

    if ($identified_to_nickserv) {
      # CONFIG: change where we announce stuff here
      my @channels = ('#monitoring', '#status');
      while (defined (my $line = <NAGIOS>)) {
        print $line;
        chomp($line);
        if ($line =~ /^\[\d+\] (HOST|SERVICE) NOTIFICATION: (.+)$/) {
            my ($type, $msg) = ($1, $2);
            my ($who, $host, $service, $state, $how, $output);
            if ($type eq "HOST") {
                ($who, $host, $state, $how, $output) = split(";",$msg,5);
                next if exists $renot{$host} && $renot{$host} >= time() - 5;
                $renot{$host} = time();

                my $id = ackable($host,undef,$state,$output);
                $msg = $state_to_color->{$state} . "$id$host is $state: $output";
            }
            else {
                ($who, $host, $service, $state, $how, $output) = split(";",$msg,6);
                next if exists $renot{"$host:$service"} && $renot{"$host:$service"} >= time() - 5;
                $renot{"$host:$service"} = time();

                my $id = ackable($host,$service,$state,$output);
                $msg = $state_to_color->{$state} . "$id$host:$service is $state: $output";
            }
            $service ||= "";

            # alerts can be separated by who they're for (which group is getting it)
#if ($who eq "mark") { push @channels, "#dw_ops", "#dw_work" }

            if ((!grep { $_ eq "$host:$service" } keys %ignore) &&
                (!grep { $_ eq "$host" } keys %ignore)) {
                $conn->privmsg($_,"$msg") foreach @channels;
            }
#[1310493492] EXTERNAL COMMAND: SCHEDULE_HOST_DOWNTIME;gearmanworker-gearmanworker3;1310492897;1310500097;1;0;28800;stumble.barr;rebooting
        } elsif ($line =~ /EXTERNAL COMMAND: SCHEDULE_HOST_DOWNTIME;(.+?);(\d+);(\d+);\d+;\d+;\d+;(.+?);(.+)$/) {
            my $ns = $3 - $2;
            $conn->privmsg($_, "Downtime (${ns}s) scheduled for host $1 by $4: $5")
                foreach @channels;
        } elsif ($line =~ /EXTERNAL COMMAND: SCHEDULE_SVC_DOWNTIME;(.+?);(.+?);(\d+);(\d+);\d+;\d+;\d+;(.+?);(.+)$/) {
            my $ns = $4 - $3;
            $conn->privmsg($_, "Downtime (${ns}s) scheduled for service $1: $2 by $5: $6")
                foreach @channels;
        } elsif ($line =~ /EXTERNAL COMMAND: (EN|DIS)ABLE_HOST_(SVC_)?NOTIFICATIONS;(.+)$/) {
            my $type = $2 ? 'All service' : 'Host';
            my $verb = $1 eq 'EN' ? "$C{G}enabled$C{N}" : "$C{R}disabled$C{N}";
            $conn->privmsg($_, "$type notifications $verb: $C{O}$3")
                foreach @channels;
        } elsif ($line =~ /EXTERNAL COMMAND: (EN|DIS)ABLE_(SVC_)?NOTIFICATIONS;(.+?);(.+)$/) {
            my $verb = $1 eq 'EN' ? "$C{G}enabled$C{N}" : "$C{R}disabled$C{N}";
            $conn->privmsg($_, "Service notifications $verb on $C{O}$3$C{N}: $C{O}$4")
                foreach @channels;
        } elsif ($line =~ /HOST DOWNTIME ALERT: (.+?);(.+?);/) {
            my $vb = { CANCELLED => 'been manually removed from', STOPPED => 'exited from', STARTED => 'entered' }->{$2};
            $conn->privmsg($_, "Host $C{O}$1$C{N} has $vb scheduled downtime.")
                foreach @channels;
        } elsif ($line =~ /SERVICE DOWNTIME ALERT: (.+?);(.+?);(.+?);/) {
            my $vb = { CANCELLED => 'been manually removed from', STOPPED => 'exited from', STARTED => 'entered' }->{$3};
            $conn->privmsg($_, "Service $C{O}$1$C{N}: $C{O}$2$C{N} has $vb scheduled downtime.")
                foreach @channels;
        }
      }

      if ((time - $laststat) > 30) {
        # we don't want to constantly stat the file or we'll hammer the disk.
        # keep it to once every 30 seconds.  Here we compare the inode of the
        # file we have open and the file at the path we're expecting and see
        # if they match.  If they don't, then the log has been rotated, so we
        # close and reopen it to pick up the new logfile.  We also only do it
        # if we don't read a line from the file, so that way we make sure
        # actually gotten to the end of the current file before we check for
        # a rotation.
        $laststat = time;
        if ((stat NAGIOS)[1] != (stat $nagioslog)[1]) {
            close NAGIOS;
            open NAGIOS, "<$nagioslog";
        }
      }
    }
}
