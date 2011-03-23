#!/bin/sh

# Any questions about setting this up, feel free to mail me at
#    NeilFred Picciotto <tnt-procmail-setup@derf.net>


# Make sure this is the correct path to the gnuclient executable.
# Also, of course, you'll need to be running gnuserv in the same emacs
# as you're running tnt.
GNUCLIENT="/usr/bin/gnuclient"


# Provided you didn't change the email code in tnt.el and properly
# used the procmail recipe which launches this script, the rest of
# this should all the right thing...

user=`echo $1 | sed -e 's/_IM_@.*$//'`

body=""
while read line; do
    body=`echo $body $line`
done

$GNUCLIENT -batch -eval \
    "(tnt-send-external-text-as-instant-message \"$user\" \"$body\")"

