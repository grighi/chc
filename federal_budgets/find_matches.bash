
rm *_matches.pdf

# Community Health Centers are "75-0350-7-1-550"

for f in *pdf; do
  year=$(echo "$f" | sed -E 's/BUDGET-([0-9]{4}).*/\1/')
  if [ "$year" -lt 1995]; then
    pattern="75–1361–0–1–550"
  else if [ "$year" -lt 2000 ]; then
    pattern="75–1362–0–1–550"
  else
    pattern="75–1362–0–1–551"
  fi
  pages=$(pdfgrep -n "$pattern" "$f" | cut -d: -f1 | sort -u | tr '\n' ',' | sed 's/,$//')
  if [ -n "$pages" ]; then
    qpdf "$f" --pages . $pages -- "${f%.pdf}_matches.pdf"
  fi
done

qpdf --empty --pages *_matches.pdf -- all_matches_combined.pdf

rm -f combined_with_headers.pdf

for f in *_matches.pdf; do
  base=$(basename "$f")

  # Create a one-page PDF header with the filename
  echo "$base" | pandoc -o header.pdf

  # Merge header + actual PDF
  qpdf --empty --pages header.pdf "$f" -- temp_combined.pdf

  # Append to master file
  if [ -f combined_with_headers.pdf ]; then
    qpdf --empty --pages combined_with_headers.pdf temp_combined.pdf -- combined_tmp.pdf
    mv combined_tmp.pdf combined_with_headers.pdf
  else
    mv temp_combined.pdf combined_with_headers.pdf
  fi

  rm header.pdf
done

rm -f temp_combined.pdf
