module post_prmtr
use prmtr
implicit none
   integer,parameter :: bis=xin
   integer,parameter :: bie=xMax
   integer,parameter :: bjs=yin
   integer,parameter :: bje=yMax
   integer,parameter :: bks=zin
   integer,parameter :: bke=zMax
   integer,parameter :: ni=bie-1
   integer,parameter :: nj=bje-1
   integer,parameter :: nk=bke-1
end module post_prmtr
