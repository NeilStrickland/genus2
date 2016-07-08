save_dir := cat(genus2_dir,"/data_toy");

build_data := table();
save_data := table();
load_data := table();

######################################################################

build_data["HP_table"] := proc()
 global HP_table;
 local a0,i;

 userinfo(5,genus2,"Building HP_table");

 HP_table := `new/HP_table`():
 HP_table["H_to_P_num_samples"] := 20;
 HP_table["H_to_P_poly_deg"] := 15;
 HP_table["H_to_P_num_steps"] := 20;

 for a0 in [0.7,0.8,0.9,0.6,0.5,0.4,0.35,0.3,0.25,0.2,0.15,0.1] do
  HP_table["add_a_H",a0];
 od:

 HP_table["set_spline"];

 HP_table["P_to_H_poly_deg"] := 20;
 HP_table["P_to_H_num_charts"] := 10;
 HP_table["P_to_H_tolerance"] := 10.^(-20);
 HP_table["P_to_H_gap"] := 0.02;

 for i from 1 to 9 do
  HP_table["add_a_P",0.1 * i];
 od;

 NULL;
end:

##################################################

save_data["HP_table"] := proc()
 save(HP_table,cat(save_dir,"/hyperbolic/HP_table.m"));
end:

load_data["HP_table"] := proc()
 read(cat(save_dir,"/hyperbolic/HP_table.m"));
end:

######################################################################

build_data["triangle_quadrature_rule"] := proc() 
 local i;
 global dunavant_19;

 userinfo(5,genus2,"Building triangle quadrature rule");

 dunavant_19 := 
  `new/triangle_quadrature_rule`(19,
  [[1/3, 1/3, 1/3], [3151/151636, 12629/25794, 12629/25794],
   [8689/95561, 43436/95561, 43436/95561],
   [8267/41929, 16831/41929, 16831/41929],
   [5570/11393, 5823/22786, 5823/22786],
   [12479/19322, 6843/38644, 6843/38644],
   [30657/39310, 8653/78620, 8653/78620], 
   [38509/43320, 8362/150589, 8362/150589], 
   [31895/32721, 2623/207814, 2623/207814], 
   [2479/686434, 13331/33685, 15921/26507], 
   [13675/101698, 21427/69584, 24205/43409], 
   [1147/79399, 21563/81503, 14171/19655], 
   [4995/106427, 13589/37901, 40215/67642], 
   [1712/598367, 13505/85579, 29479/35122], 
   [4042/53857, 11153/49821, 18623/26563], 
   [9045/261061, 36764/258135, 18130/22031], 
   [2271/223499, 24189/369328, 24069/26039]],
  [1251/38017, 4175/404134, 2879/128600, 2858/94429, 8053/264111, 9113/377206, 16861/1050477, 3191/394702, 1189/571810, 1878/483413, 9199/359699, 5368/604443, 3015/186982, 92/36919, 11344/621833, 9332/909679, 2254/593169]
):

 userinfo(6,genus2,"Improving triangle quadrature rule");
 for i from 1 to 4 do dunavant_19["improve"]; od:
end:

##################################################

save_data["triangle_quadrature_rule"] := proc()
 save(dunavant_19,cat(save_dir,"/quadrature/dunavant_19.m"));
end:

load_data["triangle_quadrature_rule"] := proc()
 read(cat(save_dir,"/quadrature/dunavant_19.m"));
end:

######################################################################

build_data["grid"] := proc()
 global rational_grid_dunavant_19;
 local i,j,c,EE,E,FF,F;

 userinfo(5,genus2,"Creating grid");

 rational_grid_dunavant_19 := `new/E_grid`():
 rational_grid_dunavant_19["triangle_quadrature_rule"] :=
  eval(dunavant_19);

 userinfo(7,genus2,"Adding vertices");
 for i from 0 to 4 do
  for j from 0 to 6 do
   if i = 0 then
    if j = 0 then
     c := CONSTRAINT_V6;
    elif j = 6 then
     c := CONSTRAINT_V0;
    else
     c := CONSTRAINT_C1;
    fi;
   elif i = 4 then
    if j = 0 then
     c := CONSTRAINT_V3;
    elif j = 6 then
     c := CONSTRAINT_V11;
    else
     c := CONSTRAINT_C3;
    fi;
   else
    if j = 0 then
     c := CONSTRAINT_C0;
    elif j = 6 then
     c := CONSTRAINT_C5;
    else
     c := CONSTRAINT_FREE;
    fi;
   fi;
   rational_grid_dunavant_19["add_new_point",c,evalf(rational_grid_points[i+1,j+1])];
  od:
 od:

 userinfo(7,genus2,"Adding edges");
 EE := [
  seq(seq([7*i+j,7*i+j+1],j=0..5),i=0..4),
  seq(seq([7*i+j,7*i+j+7],j=0..6),i=0..3),
  seq(seq([7*i+j,7*i+j+8],j=0..2),i=0..1),
  seq(seq([7*i+j,7*i+j+6],j=4..6),i=0..1),
  seq(seq([7*i+j,7*i+j+6],j=1..3),i=2..3),
  seq(seq([7*i+j,7*i+j+8],j=3..5),i=2..3)
 ]:

 for E in EE do rational_grid_dunavant_19["add_new_edge",op(E)]; od:

 userinfo(7,genus2,"Adding faces");
 FF := [
  seq(seq([7*i+j  ,7*i+j+1,7*i+j+8],j=0..2),i=0..1),
  seq(seq([7*i+j  ,7*i+j+7,7*i+j+8],j=0..2),i=0..1),
  seq(seq([7*i+j-1,7*i+j  ,7*i+j+6],j=4..6),i=0..1),
  seq(seq([7*i+j  ,7*i+j+6,7*i+j+7],j=4..6),i=0..1),
  seq(seq([7*i+j-1,7*i+j  ,7*i+j+6],j=1..3),i=2..3),
  seq(seq([7*i+j  ,7*i+j+6,7*i+j+7],j=1..3),i=2..3),
  seq(seq([7*i+j  ,7*i+j+1,7*i+j+8],j=3..5),i=2..3),
  seq(seq([7*i+j  ,7*i+j+7,7*i+j+8],j=3..5),i=2..3)
 ];

 for F in FF do rational_grid_dunavant_19["add_new_face",op(F)]; od:

 userinfo(7,genus2,"Creating samples");
 rational_grid_dunavant_19["create_samples"];

 NULL;
end:

##################################################

save_data["grid"] := proc()
 save(rational_grid_dunavant_19,
      cat(save_dir,"/embedded/roothalf/rational_grid_dunavant_19.m"));
end:

load_data["grid"] := proc()
 read(cat(save_dir,"/embedded/roothalf/rational_grid_dunavant_19.m"));
end:

######################################################################

build_data["E_quadrature_rule"] := proc()
 local P,Q,PP;
 global quadrature_frobenius_25a;

 userinfo(5,genus2,"Creating quadrature rule for EX^*");

 Q := `new/E_quadrature_rule`():

 userinfo(6,genus2,"Adding points");

 PP := [
   [0, 0, 1, 0], 
   [0, 1, 0, 0], 
   [33461/47321, 33461/47321, 0, 0],
   [0, 0, 38804/47525, -29681/51409], 
   [11608/37891, 34/5004131, 23011/26164, -23191/63676],
   [-8093/61002, 453/1347455, -31471/38190, 16085/29206],
   [43537/51554, 0, -35081/80224, -19457/62925],
   [0, 7351/30813, 30967/36647, 20441/42711], 
   [7687/17700, -8639/22381, 85585/109973, 11944/50141], 
   [-26014/45893, -26014/45893, 17794/29765, 0], 
   [41089/44864, 11738/29235, 0, 0], 
   [55299/169997, -24344/42595, 15046/22279, -45463/136178], 
   [9800/11707, -71234/130217, 0, 0], 
   [-18143/35771, 0, 27091/38499, 44363/89158], 
   [-17405/82067, -22296/54959, 64687/85332, -36185/77897], 
   [-26688/188789, 128852/137569, 48447/182600, -3378/18785], 
   [21589/32500, 21589/32500, -21844/63733, 0], 
   [10433/34642, -23993/28198, -30464/81355, 35581/167562], 
   [-3593/19589, -18783/26590, 27289/47592, -40265/108161], 
   [-30110/124239, -30110/124239, -24862/26465, 0], 
   [39464/40765, 22915/91433, 0, 0], 
   [95687/216048, -25784/34071, -9499/21149, 10495/61203], 
   [-27711/91097, 0, -31868/34245, 8386/41179], 
   [-22178/52465, 22178/52465, 19830/24737, 0], 
   [-23657/34665, 44459/84130, -91766/185619, -26023/252859]]:

 for P in PP do Q["add_new_point",P]: od:

 userinfo(6,genus2,"Adding test monomials");
 Q["set_max_deg",7,rational_grid_dunavant_19];

 userinfo(6,genus2,"Solving for weights");
 Q["solve_weights"]: 

 userinfo(6,genus2,"Adjusting to increase evaluation determinant");
 Q["increase_eval_det",10.^(-4)];

 userinfo(6,genus2,"Adjusting to reduce errors");
 Q["reduce_errors"];

 quadrature_frobenius_25a := eval(Q):
 NULL;

end:

##################################################

save_data["E_quadrature_rule"] := proc()
 save(quadrature_frobenius_25a,
      cat(save_dir,"/embedded/roothalf/quadrature_frobenius_25a.m"));
end:

load_data["E_quadrature_rule"] := proc()
 read(cat(save_dir,"/embedded/roothalf/quadrature_frobenius_25a.m"));
end:

######################################################################

build_data["square_diffeo"] := proc()
 userinfo(5,genus2,"Tabulating values of square_diffeo_E0_inverse");

 find_square_diffeo_E0_inverse(24);
end:

##################################################

save_data["square_diffeo"] := proc()
 save_square_diffeo_E0_inverse();
end:

load_data["square_diffeo"] := proc()
 load_square_diffeo_E0_inverse();
end:

######################################################################

build_data["EH_atlas",0] := proc()
 build_data["EH_atlas",1]();
 build_data["EH_atlas",2]();
 build_data["EH_atlas",3]();
 build_data["EH_atlas",4]();
end:

build_data["EH_atlas",1] := proc()
 global EH_atlas;

 userinfo(5,genus2,"Creating EH_atlas");

 EH_atlas := `new/EH_atlas`():
 EH_atlas["quadrature_rule"] := eval(quadrature_frobenius_25a):
end:

build_data["EH_atlas",2] := proc()
 local S,T,E,d,a,b,i,j,k,s,t,e;
 global EH_atlas;

 d := 10;

 userinfo(6,genus2,"Finding rescaling function");
 EH_atlas["find_rescale_pade_alt",4]:

 userinfo(6,genus2,"Finding a_H");
 EH_atlas["find_a_H"];

 set_a_H0(EH_atlas["a_H"]):

 userinfo(6,genus2,"Finding curve parameters");
 EH_atlas["find_u",10];

 T[0] := [32/219, 23/64, 66/113, 129/157]:
 T[1] := [61/69, 16/25, 13/18, 89/110, 51/116, 35/69, 95/164, 31/113, 52/157, 
	  61/156, 26/983, 26/331, 117/901, 33/182, 83/355, 29/691]:
 T[3] := [29/36, 75/83, 61/99, 123/175, 103/231, 47/90, 23/64, 35/167,
          67/244, 5/39, 31/676]:
 T[5] := [22/491, 131/1201, 73/409, 43/176, 61/189, 89/217, 115/234,
          250/417, 75/104, 163/198, 129/134]:

 S :=[[16/333, 5/456], [62/135, 9/1724], [49/166, 88/2933], [39/271, 23/279],
      [17/465, 49/292], [40/49, 7/2145], [308/431, 6/337], [122/209, 25/493],
      [43/100, 20/183], [269/999, 39/196], [34/269, 17/53], [27/919, 113/241],
      [53/55, 3/1237], [53/57, 17/1588], [71/81, 127/4254], [95/119, 41/620],
      [110/159, 49/388], [77/138, 49/227], [32/79, 35/104], [1164/4657, 44/91],
      [32/277, 67/104], [19/730, 155/194], [2312/2313, 1/6762], [420/421, 3/2252],
      [127/128, 1/185], [101/103, 8/521], [93/97, 6/169], [59/64, 76/1063],
      [83/96, 24/185], [147/188, 19/88], [84/125, 143/430], [50/93, 271/569],
      [47/121, 75/118], [331/1379, 53/67], [91/810, 56/61], [19/723, 108/109],
      [2621/2622, 9/2018], [415/416, 7/517], [121/122, 3/94], [47/48, 9/139],
      [150/157, 13/110], [76/83, 68/343], [53/62, 96/311], [10/13, 30/67],
      [27/41, 103/170], [39/74, 13/17], [71/185, 107/119], [68/281, 59/60],
      [570/571, 28/603], [132/133, 35/379], [47/48, 98/599], [83/87, 191/720],
      [114/125, 39/98], [45/53, 66/119], [129/169, 74/103], [59/90, 109/126],
      [44/83, 29/30], [1321/1322, 15/131], [169/170, 52/255], [51/52, 52/159],
      [127/133, 133/277], [83/91, 63/97], [147/173, 73/90], [56/73, 73/78],
      [272/273, 45/118], [61/62, 57/103], [254/265, 125/171], [232/253, 53/60],
      [133/155, 47/48], [688/689, 137/222], [83/84, 231/289], [82/85, 133/142],
      [134/135, 71/73]]:

 E := [[  0, 18],[  0, 23],[  0, 35],[  1,  7],[  1, 25],[  1, 81],[  2,  4],[  2,  8],
       [  2, 46],[  3, 34],[  3, 45],[  4,  5],[  4, 46],[  4, 50],[  5,  6],[  5, 50],
       [  5, 57],[  6,  7],[  6, 57],[  6, 67],[  7, 67],[  7, 81],[  8, 11],[  8, 46],
       [  9, 10],[  9, 14],[  9, 47],[ 10, 11],[ 10, 47],[ 10, 48],[ 11, 46],[ 11, 48],
       [ 11, 49],[ 12, 13],[ 12, 17],[ 12, 51],[ 13, 14],[ 13, 51],[ 13, 52],[ 14, 47],
       [ 14, 52],[ 14, 53],[ 15, 16],[ 15, 22],[ 15, 58],[ 16, 17],[ 16, 58],[ 16, 59],
       [ 17, 51],[ 17, 59],[ 17, 60],[ 18, 19],[ 18, 23],[ 19, 20],[ 19, 23],[ 19, 68],
       [ 20, 21],[ 20, 68],[ 20, 69],[ 21, 22],[ 21, 69],[ 21, 70],[ 22, 58],[ 22, 70],
       [ 22, 71],[ 23, 35],[ 23, 36],[ 23, 68],[ 24, 25],[ 24, 27],[ 24, 80],[ 24, 81],
       [ 24, 93],[ 25, 81],[ 26, 27],[ 26, 29],[ 26, 92],[ 26, 93],[ 26,102],[ 27, 93],
       [ 28, 29],[ 28, 30],[ 28,101],[ 28,102],[ 28,109],[ 29,102],[ 30, 32],[ 30,109],
       [ 30,114],[ 31, 32],[ 31, 33],[ 31,113],[ 31,114],[ 31,117],[ 32,114],[ 33, 34],
       [ 33,117],[ 33,118],[ 34, 45],[ 34,118],[ 35, 36],[ 36, 37],[ 36, 68],[ 36, 82],
       [ 37, 38],[ 37, 82],[ 38, 39],[ 38, 82],[ 38, 83],[ 38, 94],[ 39, 40],[ 39, 94],
       [ 39,103],[ 40, 41],[ 40,103],[ 41, 42],[ 41,103],[ 41,104],[ 41,110],[ 42, 43],
       [ 42,110],[ 42,115],[ 43, 44],[ 43,115],[ 44, 45],[ 44,115],[ 44,116],[ 44,118],
       [ 45,118],[ 46, 49],[ 46, 50],[ 47, 48],[ 47, 53],[ 47, 54],[ 48, 49],[ 48, 54],
       [ 48, 55],[ 49, 50],[ 49, 55],[ 49, 56],[ 50, 56],[ 50, 57],[ 51, 52],[ 51, 60],
       [ 51, 61],[ 52, 53],[ 52, 61],[ 52, 62],[ 53, 54],[ 53, 62],[ 53, 63],[ 54, 55],
       [ 54, 63],[ 54, 64],[ 55, 56],[ 55, 64],[ 55, 65],[ 56, 57],[ 56, 65],[ 56, 66],
       [ 57, 66],[ 57, 67],[ 58, 59],[ 58, 71],[ 58, 72],[ 59, 60],[ 59, 72],[ 59, 73],
       [ 60, 61],[ 60, 73],[ 60, 74],[ 61, 62],[ 61, 74],[ 61, 75],[ 62, 63],[ 62, 75],
       [ 62, 76],[ 63, 64],[ 63, 76],[ 63, 77],[ 64, 65],[ 64, 77],[ 64, 78],[ 65, 66],
       [ 65, 78],[ 65, 79],[ 66, 67],[ 66, 79],[ 66, 80],[ 67, 80],[ 67, 81],[ 68, 69],
       [ 68, 82],[ 69, 70],[ 69, 82],[ 69, 83],[ 70, 71],[ 70, 83],[ 70, 84],[ 71, 72],
       [ 71, 84],[ 71, 85],[ 72, 73],[ 72, 85],[ 72, 86],[ 73, 74],[ 73, 86],[ 73, 87],
       [ 74, 75],[ 74, 87],[ 74, 88],[ 75, 76],[ 75, 88],[ 75, 89],[ 76, 77],[ 76, 89],
       [ 76, 90],[ 77, 78],[ 77, 90],[ 77, 91],[ 78, 79],[ 78, 91],[ 78, 92],[ 79, 80],
       [ 79, 92],[ 79, 93],[ 80, 81],[ 80, 93],[ 82, 83],[ 83, 84],[ 83, 94],[ 84, 85],
       [ 84, 94],[ 84, 95],[ 85, 86],[ 85, 95],[ 85, 96],[ 86, 87],[ 86, 96],[ 86, 97],
       [ 87, 88],[ 87, 97],[ 87, 98],[ 88, 89],[ 88, 98],[ 88, 99],[ 89, 90],[ 89, 99],
       [ 89,100],[ 90, 91],[ 90,100],[ 90,101],[ 91, 92],[ 91,101],[ 91,102],[ 92, 93],
       [ 92,102],[ 94, 95],[ 94,103],[ 95, 96],[ 95,103],[ 95,104],[ 96, 97],[ 96,104],
       [ 96,105],[ 97, 98],[ 97,105],[ 97,106],[ 98, 99],[ 98,106],[ 98,107],[ 99,100],
       [ 99,107],[ 99,108],[100,101],[100,108],[100,109],[101,102],[101,109],[103,104],
       [104,105],[104,110],[105,106],[105,110],[105,111],[106,107],[106,111],[106,112],
       [107,108],[107,112],[107,113],[108,109],[108,113],[108,114],[109,114],[110,111],
       [110,115],[111,112],[111,115],[111,116],[112,113],[112,116],[112,117],[113,114],
       [113,117],[115,116],[116,117],[116,118],[117,118]]:

 EH_atlas["num_charts"] := 0;
 EH_atlas["charts"] := table():

 userinfo(6,genus2,"Adding vertex charts");

 EH_atlas["add_vertex_chart",0 ,d];
 EH_atlas["add_vertex_chart",3 ,d];
 EH_atlas["add_vertex_chart",6 ,d];
 EH_atlas["add_vertex_chart",11,d];

 for k in [0,1,3,5] do
  userinfo(6,genus2,sprintf("Adding charts on curve %d",k));
  a,b := op(evalf(F16_curve_limits[k]));
  for t in T[k] do
   EH_atlas["add_curve_chart",k,a + t*(b-a),d];
  od;
 od;

 userinfo(6,genus2,"Adding inner charts");

 i := 0;
 for s in S do
  EH_atlas["add_centre_chart",evalf(t_lift(s)),d];
  i := i+1;
  if mods(i,10) = 0 then
   userinfo(6,genus2,sprintf("%d inner charts added",i));
  fi;
 od;

 userinfo(6,genus2,"Adding edges");
 for e in E do EH_atlas["add_edge",op(e)]; od;
end:

build_data["EH_atlas",3] := proc()
 local S,T,E,d,a,b,i,j,k,s,t,e;
 global EH_atlas;

 userinfo(6,genus2,"set_beta_approx");
 EH_atlas["set_beta_approx"]:

 userinfo(6,genus2,"set_edge_lengths");
 EH_atlas["set_edge_lengths"]:

 userinfo(6,genus2,"optimize_beta");
 EH_atlas["optimize_beta"];

 userinfo(6,genus2,"make_H_samples");
 EH_atlas["make_H_samples",50]:

 userinfo(6,genus2,"make_D_samples");
 EH_atlas["make_D_samples",25,0.8]:

 userinfo(6,genus2,"make_H_samples_q");
 EH_atlas["make_H_samples_q"]:

 userinfo(6,genus2,"make_D_samples_q");
 EH_atlas["make_D_samples_q"]:

 NULL;
end:

build_data["EH_atlas",4] := proc()
 userinfo(6,genus2,"set_q_approx_fourier");
 EH_atlas["set_q_approx_fourier",0.8,16,6];

 userinfo(6,genus2,"set_square_q_inv");
 EH_atlas["set_square_q_inv",6];

 userinfo(6,genus2,"set_chart_dist");
 EH_atlas["set_chart_dist"];

 NULL;
end:

##################################################

save_data["EH_atlas"] := proc()
 save(EH_atlas,
      cat(save_dir,"/embedded/roothalf/EH_atlas.m"));
end:

load_data["EH_atlas"] := proc()
 read(cat(save_dir,"/embedded/roothalf/EH_atlas.m"));
 if type([EH_atlas["a_H"]],[RR0]) then
  set_a_H0(EH_atlas["a_H"]):
 fi;
end:

######################################################################

build_data["H_to_P_map"] := proc()
 global H_to_P_map;

 set_a_H0(EH_atlas["a_H"]):

 HP_table["add_a_H",a_H0]:
 H_to_P_map := eval(HP_table["H_to_P_maps"][a_H0]):
 H_to_P_map["make_samples",100]:
 H_to_P_map["set_poly_deg",20]:
 H_to_P_map["find_p1"]:
 H_to_P_map["set_p1_inv"];

 H_to_P_map["find_m_series" ,0.7,100,10]:
 H_to_P_map["find_mp_series",0.7,100,10]:
 H_to_P_map["find_p_series" ,0.7,100,10]:

 NULL;
end:

##################################################

save_data["H_to_P_map"] := proc()
 save(H_to_P_map,
      cat(save_dir,"/hyperbolic/H_to_P_map.m"));
end:

load_data["H_to_P_map"] := proc()
 read(cat(save_dir,"/hyperbolic/H_to_P_map.m"));
end:

######################################################################

build_data["P_to_H_map"] := proc()
 global P_to_H_map;

 set_a_P0(H_to_P_map["a_P"]);
 P_to_H_map := `new/P_to_H_map`():
 P_to_H_map["degree"] := 100;
 P_to_H_map["set_a_P",a_P0];
 P_to_H_map["add_charts"];
 P_to_H_map["find_p1_inv"];

 NULL;
end:

##################################################

save_data["P_to_H_map"] := proc()
 save(P_to_H_map,
      cat(save_dir,"/hyperbolic/P_to_H_map.m"));
end:

load_data["P_to_H_map"] := proc()
 read(cat(save_dir,"/hyperbolic/P_to_H_map.m"));
end:

######################################################################

build_data["E_to_S_map"] := proc()
 global E_to_S_map,EH_atlas;

 EH_atlas["H_to_P_map"] := eval(H_to_P_map):
 EH_atlas["P_to_H_map"] := eval(P_to_H_map):

 E_to_S_map := `new/E_to_S_map`():
 E_to_S_map["find_p",EH_atlas]:

 NULL;
end:

##################################################

save_data["E_to_S_map"] := proc()
 save(E_to_S_map,
      cat(save_dir,"/embedded/roothalf/E_to_S_map.m"));
end:

load_data["E_to_S_map"] := proc()
 read(cat(save_dir,"/embedded/roothalf/E_to_S_map.m"));
end:

######################################################################

#build_data["HP_table"]();
#save_data["HP_table"]();
load_data["HP_table"]();

#build_data["triangle_quadrature_rule"]();
#save_data["triangle_quadrature_rule"]();
load_data["triangle_quadrature_rule"]();

#build_data["grid"]();
#save_data["grid"]();
load_data["grid"]();

#build_data["E_quadrature_rule"]();
#save_data["E_quadrature_rule"]();
load_data["E_quadrature_rule"]();

#build_data["square_diffeo"]();
#save_data["square_diffeo"]();
#load_data["square_diffeo"]();

#build_data["EH_atlas",1]();
#build_data["EH_atlas",2]();
#build_data["EH_atlas",3]();
#build_data["EH_atlas",4]();
#save_data["EH_atlas"]();
#load_data["EH_atlas"]();

#build_data["H_to_P_map"]();
#save_data["H_to_P_map"]();
#load_data["H_to_P_map"]();

#build_data["P_to_H_map"]();
#save_data["P_to_H_map"]();
#load_data["P_to_H_map"]();

#build_data["E_to_S_map"]();
#save_data["E_to_S_map"]();
#load_data["E_to_S_map"]();

