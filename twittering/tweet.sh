#!/bin/sh
#set -x
alias wget="wget -q -U 'Mozilla/5.0 (Android; Linux armv7l; rv:5.0) Gecko/20110615 Firefox/5.0 Fennec/5.0' --keep-session-cookies --load-cookies cookies_$1.txt --save-headers"

#AUTH_TOKEN=`wget -O- https://mobile.twitter.com | grep new_tweet | cut -d'"' -f16`
AUTH_TOKEN=`cat auth_token_$1.txt`

wget -O /dev/null --referer=https://mobile.twitter.com/ --post-data "authenticity_token=$AUTH_TOKEN&tweet%5Btext%5D=$2&tweet%5Bin_reply_to_status_id%5D=&tweet%5Blat%5D=&tweet%5Blong%5D&tweet%5Bplace_id%5D=&tweet%5Bdisplay_coordinates%5D=&ctx=home" https://mobile.twitter.com/
# | grep new_tweet | cut -d'"' -f16 > auth_token_$1.txt
