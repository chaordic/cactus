#! /bin/bash
# Configure git
git config user.email "platform@chaordicsystems.com"
git config user.name "circle-ci"

echo "Fetch remote tags"
git fetch --tags --all --prune

# Generate version number
latest_tag=$(git tag -l | sort -g | tail -n1)
new_version=$((${latest_tag:-0}+1))

echo "Changing build.gradle"
sed -i "s/^version = .*$/version = $new_version/g" build.gradle

echo "Commiting build.gradle file"
git commit build.gradle -m "Change build.gradle version to $new_version [ci skip]"

echo "Creating tag $new_version"
git tag $new_version -m "Tag for version $new_version"