module NBody where

import Data.AEq
import Vectors

{-
N-Body Problem:
In physics, the n-body problem is the problem of predicting the individual motions
of a group of celestial objects interacting with each other gravitationally.
The problem remains unsolved and the way to observe the interactions between the bodies is to
simulate the systems.

The problem is first solved in O(N^2) using lists and later optimised to O(NlogN) using our Quadtrees module.
-}

-- Defining the types
type Velocity = Vector

type Accel = Vector

type Mass = Double

type TimeStep = Double

data Body = Body {p :: Point, v :: Velocity, m :: Mass} -- Body has a location (x,y), velocity and mass
  deriving (Show, Eq)

{-
The equations:

1. F = (G m1 m2)/ r^2

2. a = (G m2 )/ r^2

3. s` = s + ut + 1/2 a t^2

4. v = u + at

where
  G = Gravitational constant -> 6.67 x 10^(-11)
  F = Force
  a = Acceleration
  r = Distance between the two bodies
  s = Initial Position
  s` = Final Position
  u = Initial velocity
  v = Final velocity
  t = TimeStep
-}

sun :: Body
sun = Body (0.0, 0.0) (0.0, 0.0) 2.0e30

mercury :: Body
mercury = Body (579e8, 0.0) (0.0, 47400.0) 3.30e23

venus :: Body
venus = Body (0.0, 1082e8) (-35000.0, 0.0) 4.87e24

earth :: Body
earth = Body (-1496e8, 0.0) (0.0, -29800.0) 5.98e24

mars :: Body
mars = Body (0.0, -2279e8) (24100.0, 0.0) 6.42e23

jupiter :: Body
jupiter = Body (7786e8, 0.0) (0.0, 13100.0) 1.90e27

saturn :: Body
saturn = Body (0.0, 14335e8) (-9700.0, 0.0) 5.69e26

uranus :: Body
uranus = Body (-28725e8, 0.0) (0.0, -6800.0) 8.69e25

planets :: [Body]
planets = [sun, mercury, venus, earth, mars, jupiter, saturn, uranus]

polarBodies :: [Body]
polarBodies = [Body (-99, -99) (2, 3) 30, Body (95, 95) (2, 3) 50, Body (98, 98) (-2, 1) 40, Body (91, 91) (2, 3) 60]

bodies1 :: [Body]
bodies1 =
  [ Body {p = (-0.5163036904744138, 1.8229243046926986), v = (1.815045258962662, -5.748520233760553e-3), m = 1.310148953516401},
    Body {p = (3.861508241451572, 1.4027279237218424), v = (1.9551522653666826, 3.891241179217837), m = 0.7569645633440127},
    Body {p = (-1.2144072615645982, 1.840816504275685), v = (1.3447570650346785, -2.646535842705206), m = 0.3495868272216051},
    Body {p = (6.883059381023463, 4.132949060124767), v = (3.8557276493941033, -2.0354977723903027), m = 7.8059964194309535},
    Body {p = (5.507988306849146, 8.323518896380918), v = (8.714485453566553, -9.885107450023147), m = 1.2250146092174674},
    Body {p = (-0.4557312100519971, 11.337660496558204), v = (-5.251081306496555, 5.683134581886249), m = 7.824728941083238},
    Body {p = (4.711088199256158, 10.959683608437821), v = (0.6149828914364878, 6.027223038993215), m = 11.274740803260482},
    Body {p = (9.360099782269769, 12.602360694917165), v = (-13.052224618205047, 10.567819563400933), m = 13.02951817727097},
    Body {p = (-6.7147609493965525, -2.02867502236722), v = (-11.146126356630313, 14.403451185336841), m = 13.351110109181782},
    Body {p = (17.420630858923033, -8.459884707767149), v = (-9.041262838154308, -18.599512735180355), m = 3.277116720385587},
    Body {p = (-0.16263111210600345, -0.9505545622613295), v = (-1.4968815892383582, -1.0981404327244408), m = 1.458589007373785},
    Body {p = (-1.662344041777977, -3.5025731923652854), v = (-1.3206828260646932, -0.5757313902543694), m = 3.069534767850656},
    Body {p = (5.19343125345705, -5.2634771421132385), v = (0.5680114703611683, 4.473795558477271), m = 3.1327248225860442},
    Body {p = (1.2638896391013388e-2, -0.47531013577830333), v = (-7.081551116673747, 6.212531252574725), m = 5.2210480123354905},
    Body {p = (9.3604247687288e-2, 8.908840737175495), v = (5.396945273529391, 5.855824170006302), m = 9.900548266241175},
    Body {p = (10.456340158381618, -1.719115476439248), v = (5.126698603288219, 5.832234589412857), m = 4.670919065976944},
    Body {p = (-10.720509792690201, 13.804059758544051), v = (-13.499992584867442, 2.063431111293386), m = 11.38515780948202},
    Body {p = (-9.1140235981606, -13.69150676270891), v = (7.255660133550497, -15.785871886668444), m = 13.194668575224293},
    Body {p = (-5.137417919592107, 7.910427256697188), v = (5.543563236009372, -11.516476934360751), m = 15.944245331646378},
    Body {p = (-19.200915945199768, -17.052923145253576), v = (14.42389373783922, 0.6377634012752429), m = 10.910036237331825},
    Body {p = (1.3387546747985855, 0.7486185015744692), v = (0.1008618319413307, -0.9315707129219944), m = 1.458801383408986},
    Body {p = (2.2711621274695406, 0.3707412738141882), v = (-1.9014799423770614, -1.7121359011476749), m = 3.1893572698091157},
    Body {p = (-5.993469685418727, 0.6526659569753985), v = (2.805560615337814, 1.728914198111111), m = 3.1281066089873266},
    Body {p = (5.385579813796588, -1.4522054537155626), v = (-1.3301666161983328, 4.336664132608805), m = 4.6924470270104324e-2},
    Body {p = (5.791630091396362, -4.282317801240223), v = (6.733076060929596, -4.154283537306625), m = 5.734353353262759},
    Body {p = (-4.088844072479884, 0.6074791715118816), v = (10.355366912008664, 9.496162430215861), m = 5.791968458134157},
    Body {p = (-13.597287178193001, 4.94942732173833), v = (-13.410315998112125, 7.403816977857033), m = 3.163130354436988},
    Body {p = (15.693064637604849, -6.5799478409707595), v = (-9.459583312947968, -3.285735892249335), m = 6.062446885427566},
    Body {p = (-14.743354228673196, -4.518649140968009), v = (-7.470846318984285, -6.566727069124929), m = 7.6476655223624075},
    Body {p = (2.7746300171204576, -4.4713635031355015), v = (11.539324767413607, -15.198203037184891), m = 18.130038265505174}
  ]

bodies2 :: [Body]
bodies2 =
  [ Body {p = (0.12553234598005694, 0.2117184289896559), v = (-0.5083016881602144, -0.7235925434272886), m = 0.9345847306845259},
    Body {p = (2.6717965350445625, -0.2355102802619659), v = (3.089283993000749, -2.083907003119109), m = 2.608189981474559},
    Body {p = (-2.7115812380079523, -3.2794238621655794), v = (-3.1811995990939597, 3.775855062079387), m = 4.138736291599652},
    Body {p = (-7.423207933068104, -4.939420471501467), v = (-6.365842586581331, 1.0324600212408463), m = 1.6294320021542086},
    Body {p = (-2.4217565579038123, -1.0614246577083524), v = (-5.5096726774093066, 6.556192523366901e-2), m = 1.5280502993399356},
    Body {p = (10.3475649893616, 8.951012590650988), v = (-10.804069085664372, -3.3463025250128444), m = 3.843993811128397},
    Body {p = (5.627335148754386, -8.225526461860778), v = (-8.180100998398486e-3, -7.828735643651158), m = 5.271636193519256},
    Body {p = (-3.7939304028230585, -5.008330619980699), v = (-1.8004054058497836, 6.230967087602735), m = 0.12651269338777743},
    Body {p = (1.0313381516808318, -1.070911022162126), v = (14.09570082352869, 3.37354252460389), m = 13.6816124864926},
    Body {p = (-2.146982528464393, 14.061182289307087), v = (1.087990062077184, 1.5069496605076345), m = 11.930566703815199},
    Body {p = (-0.2606392703354279, -1.3554412005513234), v = (-1.336249017339568, -0.5248141266747547), m = 0.27372426397499017},
    Body {p = (1.8698536461467223, -0.4670623363938647), v = (-0.3581392606645864, -2.169224812304837), m = 1.285471170210297},
    Body {p = (1.481913067843902, 5.3908449826289555), v = (-0.15906939776487342, -4.2692404974043034), m = 3.33324966213648},
    Body {p = (-5.203885521227041, -6.664880167933233), v = (-7.009161210507527, -2.5727458317032035), m = 2.7648927976916737},
    Body {p = (-2.6815307762024863, -9.64075789653426), v = (5.041649075443651, 8.949705808565776), m = 5.907473064057254},
    Body {p = (2.6039246120591395, -9.916940200148826), v = (1.0989001756333523, 8.924555047741253), m = 9.579591536179647},
    Body {p = (-1.7642184735240902, -13.4824124554199), v = (-9.198603940320702, -5.186360623308411), m = 7.715627258583567},
    Body {p = (-2.2349665956911555, 14.52212884991077), v = (3.1687434862317563, 2.5155275026281156), m = 3.2269411967934545},
    Body {p = (-11.973424863986788, 9.749816943703491), v = (17.18002700862578, 14.873028970648953), m = 17.951766237835557},
    Body {p = (-0.3186013463069334, -3.1354462954602242), v = (1.2198405357433237, 7.1310564432664965), m = 6.412917012994099},
    Body {p = (1.6465966132068566, -1.0777413050846119), v = (-0.5135417966819901, -1.0079018097892518), m = 1.9698939085933806},
    Body {p = (-3.6437151991056798, 0.9094987434750061), v = (3.8599913507694805, 1.2739738442055288), m = 2.111878866347584},
    Body {p = (2.49886165750548, 0.1363005794159732), v = (0.8653719355336319, -4.717892843440026), m = 2.453761554493919},
    Body {p = (6.093411815973619, -7.9621940739706), v = (-7.632776141895817, -7.413459846474288), m = 0.7020994811953383},
    Body {p = (9.260653867204075, 0.14162936848476193), v = (7.901292074155962, 6.024908473305709), m = 4.4275168767322235},
    Body {p = (-3.6857388112420733, 8.286345215774192), v = (4.80530862862234, -7.907244009107652), m = 9.469496896967101},
    Body {p = (-12.188000927265227, -4.885922652111931), v = (-8.628909438878786, 12.947901404595262), m = 4.253332204334106},
    Body {p = (-2.690441255656043, -3.3667027331402926), v = (2.172807137924936, -8.921990165319178), m = 7.16688265890612},
    Body {p = (-14.517639358723875, 5.765540294484024), v = (7.563479984912729, -9.780367129565123), m = 8.32151267491008},
    Body {p = (19.024896233623604, -4.808235652890525), v = (-0.530436991498432, 16.10413107003652), m = 11.453725345476451}
  ]

almostEqualBodies :: Body -> Body -> Bool
almostEqualBodies b1 b2 = almostEqualPoints (p b1) (p b2) && almostSameVector (v b1) (v b2) && (m b1) ~== (m b2)

g :: Double -- Gravitational Constant
g = 6.67e-11

force :: Body -> Body -> Double
force b1@(Body p1 _ m1) b2@(Body p2 _ m2) = (g * m1 * m2) / (distancePQ p1 p2) ^ 2

acceleration :: Body -> Body -> Accel
acceleration b1@(Body p1 u1 m1) b2@(Body p2 u2 m2) =
  if b1 == b2
    then zeroVector
    else scaleVector (force b1 b2 / m1) (unitVector (vectorPQ p1 p2))

{-
Test: G M m/ r^2 ~ g*m

where
  G = Gravitation constant
  g = acceleration due to gravity ~ 9.8 m/s^2
  m = mass of a person standing on surface of earth
-}

person :: Body
person = Body (6.38e6, 0) (0, 0) 70

earthAsCenter :: Body
earthAsCenter = Body (0, 0) (0, 0) 5.98e24

updateVelocity :: Velocity -> TimeStep -> Accel -> Velocity
updateVelocity u t a = u `addVectors` (scaleVector t a)

updatePosition :: Point -> Velocity -> TimeStep -> Accel -> Point
updatePosition p u t a = p `addVectors` (scaleVector t u) `addVectors` (scaleVector ((0.5) * (t ^ 2)) a)

updateBody :: Body -> TimeStep -> Accel -> Body
updateBody (Body p u m) t a = Body (updatePosition p u t a) (updateVelocity u t a) m

netAcc :: Body -> [Body] -> Accel
netAcc b = foldr (addVectors . acceleration b) zeroVector

moveBodies :: TimeStep -> TimeStep -> TimeStep -> [Body] -> [Body]
moveBodies t dT oneS bs = foldr (\a acc -> moveOnce acc) bs [1 .. oneS]
  where
    moveOnce bodies = map (\b -> updateBody b (t * dT) (netAcc b bodies)) bodies

runForTime :: Double -> [Body] -> [Body]
runForTime t bs =
  if t <= 0
    then bs
    else runForTime (t - 1) nextbs
  where
    nextbs = moveBodies 1 1 1 bs