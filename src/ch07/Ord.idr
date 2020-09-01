record Album where
  constructor MkAlbum
  artist  : String
  title   : String
  year    : Integer

Eq Album where
  (==) (MkAlbum artist title year) (MkAlbum artist' title' year') =
    artist == artist' && title == title' && year == year'

Ord Album where
  compare (MkAlbum artist title year) (MkAlbum artist' title' year') =
    case compare artist artist' of
      EQ => case compare title title' of
        EQ => compare year year'
        diff_title => diff_title
      diff_artist => diff_artist

Show Album where
  show (MkAlbum artist title year) = "\"" ++ title ++ "\" by " ++ artist ++ " (" ++ show year ++ ")"
  
help : Album
help = MkAlbum "The Beatles" "Help" 1965

rubbersoul : Album
rubbersoul = MkAlbum "The Beatles" "Rubber Soul" 1965

clouds : Album
clouds = MkAlbum "Joni Mitchell" "Clouds" 1969

hunkydory : Album
hunkydory = MkAlbum "David Bowie" "Hunky Dory" 1971

heroes : Album
heroes = MkAlbum "David Bowie" "Heroes" 1977

collection : List Album
collection = [help, rubbersoul, clouds, hunkydory, heroes]

test : List String
test = map show (sort collection)