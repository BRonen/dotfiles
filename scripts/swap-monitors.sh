#!/usr/bin/env bash
set -euo pipefail

left_monitor=1
right_monitor=2

left_workspaces="$(aerospace list-workspaces --monitor "$left_monitor" --empty no)"
right_workspaces="$(aerospace list-workspaces --monitor "$right_monitor" --empty no)"

while IFS= read -r ws; do
  [ -z "$ws" ] && continue
  aerospace move-workspace-to-monitor --workspace "$ws" "$right_monitor" || true
done <<< "$left_workspaces"

while IFS= read -r ws; do
  [ -z "$ws" ] && continue
  aerospace move-workspace-to-monitor --workspace "$ws" "$left_monitor" || true
done <<< "$right_workspaces"
