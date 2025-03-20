#!/bin/bash
set -e

echo "Generating documentation index..."

# Remove any existing index.html in the current directory (gh-pages branch root)
rm -f index.html

# Write the header of the index.html file with responsive design and adaptive colors.
cat > index.html <<'EOF'
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>haskell-socket-unix Documentation Versions</title>
  <style>
    /* Base styling */
    body {
      margin: 0;
      padding: 20px;
      font-family: "Source Code Pro", monospace;
      line-height: 1.6;
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
      background-color: #ddd;
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
    }
    /* Light mode */
    @media (prefers-color-scheme: light) {
      body {
        background-color: #ffffff;
        color: #333333;
      }
      .badge {
        background-color: #eee;
        color: #333;
      }
    }
    /* Dark mode */
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
    /* Responsive tweaks */
    @media (max-width: 600px) {
      h1 {
        font-size: 1.5em;
      }
      .badge {
        font-size: 0.8em;
        padding: 4px 8px;
      }
    }
    /* Inline SVG logo styling */
    .logo {
      width: 60px;
      height: auto;
      vertical-align: middle;
      margin-right: 10px;
    }
    .header-title {
      display: flex;
      align-items: center;
      flex-wrap: wrap;
    }
  </style>
</head>
<body>
  <div class="header-title">
    <!-- Inline SVG: A Haskell lambda combined with a simple plug icon -->
    <svg class="logo" viewBox="0 0 64 64" xmlns="http://www.w3.org/2000/svg">
      <!-- Lambda shape -->
      <path d="M10,54 L32,10 L54,54 Z" fill="#61dafb"/>
      <!-- Simple plug element (circle + rectangle) -->
      <circle cx="32" cy="30" r="4" fill="#282c34"/>
      <rect x="30" y="34" width="4" height="8" fill="#282c34"/>
    </svg>
    <h1>haskell-socket-unix Documentation</h1>
  </div>
  <div class="badge-bar">
    <a class="badge" href="https://hackage.haskell.org/package/socket-unix">Hackage</a>
    <a class="badge" href="https://github.com/flip111/haskell-socket-unix/blob/master/LICENSE">License MIT</a>
    <a class="badge" href="https://github.com/flip111/haskell-socket-unix/actions/workflows/ci.yml">Build Status</a>
    <a class="badge" href="https://stackage.org/package/socket-unix">Stackage LTS</a>
    <a class="badge" href="https://github.com/flip111/haskell-socket-unix/releases">GitHub Release</a>
  </div>
  <ul>
EOF

echo "Searching for version directories matching the pattern [0-9]+.[0-9]+.[0-9]+.[0-9]+" >&2
# Find directories in the current folder matching a four-number pattern (e.g. 0.2.1.0)
versions=$(find . -maxdepth 1 -type d | grep -E '^\./[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$' | sed 's|^\./||' | sort -V -r)
for ver in $versions; do
  echo "Found version directory: $ver" >&2
  # Search for the Haddock index file within the version directory.
  doc_index=$(find "$ver" -type f -path "*/doc/html/socket-unix/index.html" | head -n 1)
  if [ -n "$doc_index" ]; then
    # Remove a leading "./" if present to form a relative URL.
    rel_url=$(echo "$doc_index" | sed 's|^\./||')
    echo "    <li><a href=\"$rel_url\">Documentation for $ver</a></li>" >> index.html
  else
    echo "    <li>$ver: Documentation not found</li>" >> index.html
  fi
done

# Append the footer of the HTML file
cat >> index.html <<'EOF'
  </ul>
  <footer>
    <p>&copy; 2025 haskell-socket-unix. Built with love and Haskell.</p>
  </footer>
</body>
</html>
EOF

echo "Documentation index generated successfully at index.html."
