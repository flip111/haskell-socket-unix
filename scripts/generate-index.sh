#!/bin/bash
set -e

echo "Generating documentation index..."

# Debug: List all directories in the current directory.
echo "DEBUG: Listing all directories at maxdepth=1:"
all_dirs=$(find . -maxdepth 1 -type d)
echo "$all_dirs"

# Filter directories that match a version pattern (e.g., 0.2.1.0).
echo "DEBUG: Filtering directories matching pattern [0-9]+\.[0-9]+\.[0-9]+\.[0-9]+:"
matching_dirs=$(echo "$all_dirs" | grep -E '^\./[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$' | sed 's|^\./||')
echo "$matching_dirs"

# Sort the matching directories in version order (descending).
echo "DEBUG: Sorting version directories in descending order:"
sorted_versions=$(echo "$matching_dirs" | sort -V -r)
echo "$sorted_versions"

# Store the sorted versions.
versions="$sorted_versions"

# Remove any existing index.html in the current directory (gh-pages branch root)
rm -f index.html

# Write the header of the index.html file.
cat > index.html <<'EOF'
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>haskell-socket-unix Documentation Versions</title>
  <style>
    /* Base styling with adaptive colors */
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
    /* Adapt to system preferences */
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
    /* Responsive design tweaks */
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

# Loop over each version directory.
echo "DEBUG: Looping over version directories:" >&2
for ver in $versions; do
  echo "DEBUG: Processing version directory: $ver" >&2
  # Search for the Haddock index file within the version directory.
  doc_index=$(find "$ver" -type f -path "*/doc/html/socket-unix/index.html" | head -n 1)
  if [ -n "$doc_index" ]; then
    # Remove a leading "./" if present to form a relative URL.
    rel_url=$(echo "$doc_index" | sed 's|^\./||')
    echo "DEBUG: Found documentation index for $ver at $rel_url" >&2
    echo "    <li><a href=\"$rel_url\">Documentation for $ver</a></li>" >> index.html
  else
    echo "DEBUG: No documentation found for $ver" >&2
    echo "    <li>$ver: Documentation not found</li>" >> index.html
  fi
done

# Append the footer of the HTML file.
cat >> index.html <<'EOF'
  </ul>
  <footer>
    <p>&copy; 2025 haskell-socket-unix. Built with love and Haskell.</p>
  </footer>
</body>
</html>
EOF

echo "Documentation index generated successfully at index.html."
