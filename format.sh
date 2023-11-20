fourmolu \
    -o -XQuasiQuotes \
    -o -XTemplateHaskell \
    -o -XTypeApplications \
    -o -XImportQualifiedPost \
    -o -XPatternSynonyms \
    -o -XOverloadedRecordDot \
    -m inplace $(git ls-files '*.hs')
