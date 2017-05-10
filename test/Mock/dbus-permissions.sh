#!/bin/bash
mkdir -p /etc/dbus-1
cat << EOF > /etc/dbus-1/system-local.conf
<!DOCTYPE busconfig PUBLIC "-//freedesktop//DTD D-Bus Bus Configuration 1.0//EN" "http://www.freedesktop.org/standards/dbus/1.0/busconfig.dtd">
<busconfig>
  <policy user="*">
  <allow own="org.bluez.Mock"/>
  <allow send_destination="org.bluez.Mock"/>
  </policy>
</busconfig>
EOF
