#!/bin/sh
set -ex

# ---------------------------
# Config options, to override any of these, set them in .local.conf
[ -e ".local-conf" ] && . ./.local-conf

PROJECT_PATH="${PROJECT_PATH-$(dirname "$(readlink -f "$0")")}"  # The full project path, e.g. /srv/xxx
PROJECT_NAME="${PROJECT_NAME-$(basename "${PROJECT_PATH}")}"  # The project directory name, e.g. xxx
PROJECT_MODE="${PROJECT_MODE-development}"  # The project mode, development or production

SERVICE_USER="${SERVICE_USER-ffdb_dlmtool_worker}"

if [ "${PROJECT_MODE}" = "development" ]; then
    SERVICE_INSTANCES="${SERVICE_INSTANCES-}"
else
    SERVICE_INSTANCES="${SERVICE_INSTANCES-1 2 3}"
fi

adduser --system --no-create-home --group ${SERVICE_USER}

# -----------------
# Systemd unit file

systemctl stop --all "${PROJECT_NAME}@*.service"
systemctl list-units --no-pager --no-legend -a "${PROJECT_NAME}@*.service" \
    | cut -d' ' -f1 \
    | xargs --no-run-if-empty systemctl disable

cat <<EOF > "/etc/systemd/system/${PROJECT_NAME}.env"
EOF
chmod 600 -- "/etc/systemd/system/${PROJECT_NAME}.env"
cat <<EOF > "/etc/systemd/system/${PROJECT_NAME}@.service"
[Unit]
Description=${PROJECT_NAME} instance %I
After=network.target

[Service]
ExecStart=$(command -v Rscript) ${PROJECT_PATH}/worker.R
WorkingDirectory=${PROJECT_PATH}
StateDirectory=${PROJECT_NAME}/%i
User=${SERVICE_USER}
Group=${SERVICE_USER}
EnvironmentFile=/etc/systemd/system/${PROJECT_NAME}.env
Restart=on-failure
RestartSec=5s

[Install]
WantedBy=multi-user.target
EOF

systemctl daemon-reload
for x in ${SERVICE_INSTANCES}; do
    systemctl enable "${PROJECT_NAME}@${x}.service"
    systemctl start "${PROJECT_NAME}@${x}.service"
done
