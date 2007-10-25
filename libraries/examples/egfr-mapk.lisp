(in-package b-user)

(include b-user/3d-ode-biochem)
(include (@folder/locations/cell
          @folder/reactions/mapk-cascade-k1
          @folder/reactions/egfr-egf-k1 
          @folder/reactions/mapk-cascade-k1))


cell.membrane.(contains egfr)
cell.inner.(contains mapk)
cell.inner.(contains mapkk)
cell.inner.(contains mapkkk)

dish.(contains egf)

{egfr.t0 := 1.1 millimoles}
{egf.t0 := 1 millimoles}
{mapk.t0 := 1.2 millimoles}
{mapkk.t0 := 1.3 millimoles}
{mapkkk.t0 := 1.4 millimoles}
