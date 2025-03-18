#!/bin/bash
set -e

echo "Generating documentation index..."

# Remove any existing index.html in the current directory (gh-pages branch root)
rm -f index.html

# Write the header of the index.html file
cat > index.html <<'EOF'
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>haskell-socket-unix Documentation Versions</title>
  <style>
    body { font-family: sans-serif; padding: 20px; }
    h1 { font-size: 2em; }
    ul { list-style: none; padding: 0; }
    li { margin-bottom: 10px; }
    a { text-decoration: none; color: #0366d6; }
    a:hover { text-decoration: underline; }
  </style>
</head>
<body>
  <h1>haskell-socket-unix Documentation</h1>
  <ul>
EOF

# Loop over version directories (assumed to be in the gh-pages branch root and named like "v*")
versions=$(find . -maxdepth 1 -type d -name "v*" | sort -V -r)
for ver in $versions; do
  ver=$(basename "$ver")
  # Search for the Haddock index file within the version directory.
  # This assumes the index is located somewhere with the path pattern "*/doc/html/socket-unix/index.html"
  doc_index=$(find "$ver" -type f -path "*/doc/html/socket-unix/index.html" | head -n 1)
  if [ -n "$doc_index" ]; then
    # Remove a leading "./" if present to form a relative URL
    rel_url=$(echo "$doc_index" | sed 's|^\./||')
    echo "    <li><a href=\"$rel_url\">Documentation for $ver</a></li>" >> index.html
  else
    echo "    <li>$ver: Documentation not found</li>" >> index.html
  fi
done

# Append the footer of the HTML file
cat >> index.html <<'EOF'
  </ul>
</body>
</html>
EOF

echo "Documentation index generated successfully at index.html."
