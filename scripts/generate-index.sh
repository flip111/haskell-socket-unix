#!/bin/bash
set -euo pipefail

# DEBUG logging function
debug() {
  echo "DEBUG: $*" >&2
}

# Function: get_version_directories
# Returns all directories in the current folder.
get_version_directories() {
  find . -maxdepth 1 -type d
}

# Function: filter_version_directories
# Filters the list to only include directories matching a four-number version pattern (e.g. 0.2.1.0).
filter_version_directories() {
  grep -E '^\./[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$' | sed 's|^\./||'
}

# Function: sort_versions
# Sorts version strings in descending (version) order.
sort_versions() {
  sort -V -r
}

# Function: find_doc_index
# Given a version directory, returns the path to the documentation index file.
# First it checks if "index.html" exists directly in the directory.
# If not, it searches recursively for a file named "index.html".
find_doc_index() {
  local ver="$1"
  local index_path=""
  if [ -f "$ver/index.html" ]; then
    index_path="$ver/index.html"
  else
    index_path=$(find "$ver" -type f -name "index.html" | head -n 1)
  fi
  echo "$index_path"
}

# Function: generate_index_html
# Generates the index.html file using the sorted version directories.
generate_index_html() {
  local versions_sorted="$1"
  # Remove any existing index.html
  rm -f index.html

  # Write the HTML header
  cat > index.html <<'EOF'
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>haskell-socket-unix Documentation Versions</title>
  <style>
    /* Base styling with adaptive colors and responsive design */
    body {
      margin: 0;
      padding: 20px;
      font-family: "Source Code Pro", monospace;
      line-height: 1.6;
      background-color: #ffffff;
      color: #333333;
    }
    h1 {
      margin-bottom: 10px;
      font-size: 2em;
    }
    .badge-bar {
      margin-bottom: 20px;
    }
    .badge {
      display: inline-block;
      padding: 5px 10px;
      margin-right: 8px;
      margin-bottom: 8px;
      border-radius: 3px;
      background-color: #eee;
      color: #333;
      text-decoration: none;
      font-size: 0.9em;
    }
    .badge:hover {
      background-color: #ccc;
    }
    ul {
      list-style: none;
      padding: 0;
    }
    li {
      margin-bottom: 10px;
    }
    a {
      text-decoration: none;
      color: #61dafb;
    }
    a:hover {
      text-decoration: underline;
    }
    footer {
      margin-top: 40px;
      font-size: 0.8em;
      text-align: center;
      color: #555;
    }
    @media (prefers-color-scheme: dark) {
      body {
        background-color: #282c34;
        color: #abb2bf;
      }
      h1 {
        color: #61dafb;
      }
      .badge {
        background-color: #3e4451;
        color: #abb2bf;
      }
      .badge:hover {
        background-color: #4b5263;
      }
    }
    @media (max-width: 600px) {
      h1 {
        font-size: 1.5em;
      }
      .badge {
        font-size: 0.8em;
        padding: 4px 8px;
      }
    }
  </style>
</head>
<body>
  <h1>haskell-socket-unix Documentation</h1>
  <div class="badge-bar">
    <a class="badge" href="https://hackage.haskell.org/package/socket-unix">Hackage</a>
    <a class="badge" href="https://github.com/flip111/haskell-socket-unix/blob/master/LICENSE">License MIT</a>
    <a class="badge" href="https://github.com/flip111/haskell-socket-unix/actions/workflows/ci.yml">Build Status</a>
    <a class="badge" href="https://stackage.org/package/socket-unix">Stackage LTS</a>
    <a class="badge" href="https://github.com/flip111/haskell-socket-unix/releases">GitHub Release</a>
  </div>
  <ul>
EOF

  # Loop over sorted versions
  debug "Looping over version directories:"
  while IFS= read -r ver; do
    debug "Processing version directory: $ver"
    local doc_index
    doc_index=$(find_doc_index "$ver")
    if [ -n "$doc_index" ]; then
      debug "Found documentation index for $ver at $doc_index"
      echo "    <li><a href=\"$doc_index\">Documentation for $ver</a></li>" >> index.html
    else
      debug "No documentation found for $ver"
      echo "    <li>$ver: Documentation not found</li>" >> index.html
    fi
  done <<< "$versions_sorted"

  # Append the footer
  cat >> index.html <<'EOF'
  </ul>
  <footer>
    <p>&copy; 2025 haskell-socket-unix. Built with love and Haskell.</p>
  </footer>
</body>
</html>
EOF
  echo "Documentation index generated successfully at index.html."
}

### Main Execution ###

debug "Starting documentation index generation..."

# Get all directories.
all_dirs=$(get_version_directories)
debug "All directories:"
echo "$all_dirs" >&2

# Filter for version directories.
matching_dirs=$(echo "$all_dirs" | filter_version_directories)
debug "Matching version directories:"
echo "$matching_dirs" >&2

# Sort the matching directories.
sorted_versions=$(echo "$matching_dirs" | sort_versions)
debug "Sorted version directories (descending order):"
echo "$sorted_versions" >&2

# Generate the index.html file using the sorted version directories.
generate_index_html "$sorted_versions"
