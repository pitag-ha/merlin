#!/bin/bash
cat > message <<EOF
This PR introduces a change in some Merlin query response(s), which has been captured by the fuzzy-test CI. The fuzzy-test diffs of the CI have been approved. Info about the diffs that have been approved:
- Distilled data:
    - Diff: https://github.com/pitag-ha/merlin/suites/$workflow_run/artifacts/$diff_id
    - Diff 256-sha: {{ steps.category_data_diff_hash.outputs.hash }}
    - Data on target branch (\`master\` at (...)): (...)
    - Data on PR merge branch, i.e. merge result of source branch (at (...)) and target branch: (...)
- Full responses:
    - Diff: (...)
    - Diff 256-sha: my-full-responses-sha
    - Data on target branch (\`master\` at (...)): (...)
    - Data on PR merge branch, i.e. merge result of source branch (at (...)) and target branch: (...)
EOF
export msg=$(cat message)

echo "$msg" | jq


# body=$( cat message | jq -Rsa . )
# echo "$body"
# curl -LsX POST -H "Authorization: $token" -v -d '{"body": "'"$body"'"}' https://api.github.com/repos/pitag-ha/merlin/issues/6/comments
# echo $?

# python
# import json, os, sys
# json.dump({"data": os.environ["msg"]}, sys.stdout)