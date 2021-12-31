
#!/bin/sh
echo "Saving output log to /var/log/great-problems/output.log"
ADDRESS=127.0.0.1
(
        cd dist &&
                ./backend \
                         --hostname=greatproblems.ca \
                         --address=$ADDRESS \
                         --port=8000 \
                         --access-log=/var/log/great-problems/access.log \
                         --error-log=/var/log/great-problems/error.log \
                &>> /var/log/great-problems/output.log
)
