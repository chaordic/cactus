#! /bin/bash
# Configure git
git config user.email "platform@chaordicsystems.com"
git config user.name "circle-ci"

new_version=$(grep -e "^version" build.gradle | awk '{print $2}')

echo "Creating tag $new_version"
git tag $new_version -m "Tag for version $new_version"