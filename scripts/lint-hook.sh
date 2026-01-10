#!/bin/bash
input=$(cat)
file_path=$(echo "$input" | jq -r '.tool_input.file_path')

if [[ "$file_path" == *.ts || "$file_path" == *.tsx ]]; then
  npx eslint --max-warnings=0 "$file_path"
fi
