#!/bin/bash
sed -i "\$i \
  <policy user=\"\*\"> \
  <allow own=\"org.bluez.Mock\"/> \
  <allow own=\"org.bluez\"/> \
  <allow send_destination=\"org.bluez.Mock\"/> \
  </policy>" /etc/dbus-1/system.conf

service dbus restart
