
#!/bin/sh
export HOME=/root/
cd /gadget/
exec 2>&1
exec chpst -u root ./run
