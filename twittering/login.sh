#!/bin/sh

wget -U "Mozilla/5.0 (Android; Linux armv7l; rv:5.0) Gecko/20110615 Firefox/5.0 Fennec/5.0" --keep-session-cookies --save-cookies cookies_$1.txt wget -O - -q --referer=https://mobile.twitter.com/session/new --post-data "username=sttm$1&password=buttsquid" https://mobile.twitter.com/session | grep new_tweet | cut -d'"' -f16 > auth_token_$1.txt
