<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>v_E[14] := [sqrt((1-a_E^2)/(1+a_E^2)/2),sqrt((1-a_E^2)/(1+a_E^2)/2),a_E*sqrt(2/(1+a_E^2)),0];
<a name="line_2"></a>for i from 1 to 7 do
<a name="line_3"></a> v_E[14+i] := act_R4[G16[1+i]](v_E[14]);
<a name="line_4"></a>od:
<a name="line_5"></a>
<a name="line_6"></a># Note: the remaining vertices are undefined for a_E > 1/sqrt(2), because they involve complex numbers.
<a name="line_7"></a>
<a name="line_8"></a>v_E[22] := [sqrt((1-2*a_E^2)/(1+2*a_E^2)/3),0,2*a_E*sqrt(2/3/(1+2*a_E^2)),-sqrt(2/3/(1+2*a_E^2))];
<a name="line_9"></a>for i from 1 to 7 do
<a name="line_10"></a> v_E[22+i] := act_R4[G16[1+i]](v_E[22]);
<a name="line_11"></a>od:
<a name="line_12"></a>
<a name="line_13"></a>v_E[30] := [sqrt((1-2*a_E^2)/(1+a_E^2))/2,sqrt((1-2*a_E^2)/(1+a_E^2))/2,a_E*sqrt(2/(1+a_E^2)),sqrt(1/2/(1+a_E^2))];
<a name="line_14"></a>for i from 1 to 15 do
<a name="line_15"></a> v_E[30+i] := act_R4[G16[1+i]](v_E[30]);
<a name="line_16"></a>od:
<a name="line_17"></a>
<a name="line_18"></a><span style="color:red">#@ num_vertices_E 
</span><a name="line_19"></a>num_vertices_E := 46:
<a name="line_20"></a>
<a name="line_21"></a>for i from 0 to num_vertices_E-1 do
<a name="line_22"></a> v_E1[i] := evalf(subs(a_E = a_E1,v_E[i]));
<a name="line_23"></a> v_E_stereo[i] := stereo(v_E1[i]);
<a name="line_24"></a> v_E_label[i] := TEXT(v_E_stereo[i],i);
<a name="line_25"></a> v_E_tangent_projector[i] := 
<a name="line_26"></a>  map(simplify,tangent_projector(v_E[i]));
<a name="line_27"></a>od:
<a name="line_28"></a>
<a name="line_29"></a><span style="color:red">#@ find_v_E_index 
</span><a name="line_30"></a>find_v_E_index := proc(x,tolerance_) 
<a name="line_31"></a> local i,d,x_float,tolerance;
<a name="line_32"></a>
<a name="line_33"></a> x_float := evalf(subs(a_E=a_E1,x));
<a name="line_34"></a> tolerance := `if`(nargs > 1, tolerance_, 0.00001);
<a name="line_35"></a> for i from 0 to num_vertices_E-1 do
<a name="line_36"></a>  if evalf(d4(v_E1[i],x_float)) < tolerance then
<a name="line_37"></a>   return(i);
<a name="line_38"></a>  fi;
<a name="line_39"></a> od;
<a name="line_40"></a> return(FAIL);
<a name="line_41"></a>end:
<a name="line_42"></a>
<a name="line_43"></a><span style="color:red">#@ find_v_E_permlist 
</span><a name="line_44"></a>find_v_E_permlist := 
<a name="line_45"></a> (phi) -> [seq(find_v_index(phi(v_E[i])),i=0..num_vertices-1)];
<a name="line_46"></a>
<a name="line_47"></a>to_cycles := proc(s) 
<a name="line_48"></a> local s1,s2,c;
<a name="line_49"></a> s1 := map(i -> i+1,s);
<a name="line_50"></a> s2 := convert(s1,disjcyc);
<a name="line_51"></a> map(c -> map(i -> i-1,c),s2);
<a name="line_52"></a>end:
<a name="line_53"></a>
<a name="line_54"></a><span style="color:red">#@ find_v_E_cycles 
</span><a name="line_55"></a>find_v_E_cycles := 
<a name="line_56"></a> (phi) -> to_cycles(find_v_E_permlist(phi));
<a name="line_57"></a>
<a name="line_58"></a>unassign('i');
<a name="line_59"></a>
<a name="line_60"></a># The results below could be computed by the function find_v_E_permlist
<a name="line_61"></a>
<a name="line_62"></a><span style="color:red">#@ v_E_permlist
</span><a name="line_63"></a>v_E_permlist[1]     := [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45]:
<a name="line_64"></a>v_E_permlist[L]     := [0,1,3,4,5,2,7,8,9,6,11,10,13,12,15,16,17,14,19,20,21,18,23,24,25,22,27,28,29,26,31,32,33,30,35,36,37,34,39,40,41,38,43,44,45,42]:
<a name="line_65"></a>v_E_permlist[LL]    := [0,1,4,5,2,3,8,9,6,7,10,11,12,13,16,17,14,15,20,21,18,19,24,25,22,23,28,29,26,27,32,33,30,31,36,37,34,35,40,41,38,39,44,45,42,43]:
<a name="line_66"></a>v_E_permlist[LLL]   := [0,1,5,2,3,4,9,6,7,8,11,10,13,12,17,14,15,16,21,18,19,20,25,22,23,24,29,26,27,28,33,30,31,32,37,34,35,36,41,38,39,40,45,42,43,44]:
<a name="line_67"></a>v_E_permlist[M]     := [1,0,2,5,4,3,9,8,7,6,12,13,10,11,18,21,20,19,14,17,16,15,26,29,28,27,22,25,24,23,34,37,36,35,30,33,32,31,42,45,44,43,38,41,40,39]:
<a name="line_68"></a>v_E_permlist[LM]    := [1,0,3,2,5,4,6,9,8,7,13,12,11,10,19,18,21,20,15,14,17,16,27,26,29,28,23,22,25,24,35,34,37,36,31,30,33,32,43,42,45,44,39,38,41,40]:
<a name="line_69"></a>v_E_permlist[LLM]   := [1,0,4,3,2,5,7,6,9,8,12,13,10,11,20,19,18,21,16,15,14,17,28,27,26,29,24,23,22,25,36,35,34,37,32,31,30,33,44,43,42,45,40,39,38,41]:
<a name="line_70"></a>v_E_permlist[LLLM]  := [1,0,5,4,3,2,8,7,6,9,13,12,11,10,21,20,19,18,17,16,15,14,29,28,27,26,25,24,23,22,37,36,35,34,33,32,31,30,45,44,43,42,41,40,39,38]:
<a name="line_71"></a>v_E_permlist[N]     := [0,1,2,5,4,3,9,8,7,6,10,11,12,13,17,16,15,14,19,18,21,20,22,25,24,23,26,29,28,27,38,41,40,39,42,45,44,43,30,33,32,31,34,37,36,35]:
<a name="line_72"></a>v_E_permlist[LN]    := [0,1,3,2,5,4,6,9,8,7,11,10,13,12,14,17,16,15,20,19,18,21,23,22,25,24,27,26,29,28,39,38,41,40,43,42,45,44,31,30,33,32,35,34,37,36]:
<a name="line_73"></a>v_E_permlist[LLN]   := [0,1,4,3,2,5,7,6,9,8,10,11,12,13,15,14,17,16,21,20,19,18,24,23,22,25,28,27,26,29,40,39,38,41,44,43,42,45,32,31,30,33,36,35,34,37]:
<a name="line_74"></a>v_E_permlist[LLLN]  := [0,1,5,4,3,2,8,7,6,9,11,10,13,12,16,15,14,17,18,21,20,19,25,24,23,22,29,28,27,26,41,40,39,38,45,44,43,42,33,32,31,30,37,36,35,34]:
<a name="line_75"></a>v_E_permlist[MN]    := [1,0,2,3,4,5,6,7,8,9,12,13,10,11,19,20,21,18,17,14,15,16,26,27,28,29,22,23,24,25,42,43,44,45,38,39,40,41,34,35,36,37,30,31,32,33]:
<a name="line_76"></a>v_E_permlist[LMN]   := [1,0,3,4,5,2,7,8,9,6,13,12,11,10,20,21,18,19,14,15,16,17,27,28,29,26,23,24,25,22,43,44,45,42,39,40,41,38,35,36,37,34,31,32,33,30]:
<a name="line_77"></a>v_E_permlist[LLMN]  := [1,0,4,5,2,3,8,9,6,7,12,13,10,11,21,18,19,20,15,16,17,14,28,29,26,27,24,25,22,23,44,45,42,43,40,41,38,39,36,37,34,35,32,33,30,31]:
<a name="line_78"></a>v_E_permlist[LLLMN] := [1,0,5,2,3,4,9,6,7,8,13,12,11,10,18,19,20,21,16,17,14,15,29,26,27,28,25,22,23,24,45,42,43,44,41,38,39,40,37,34,35,36,33,30,31,32]:
<a name="line_79"></a>
<a name="line_80"></a>
<a name="line_81"></a><span style="color:red">#@ theta_E
</span><a name="line_82"></a>theta_E[1] := arctan(sqrt(1-a_E^2)/sqrt(2)/a_E);
<a name="line_83"></a>theta_E[2] := arctan(sqrt(2)*sqrt(sqrt(2)*sqrt(1-a_E^2)-1)*(sqrt(1-a_E^2)+sqrt(2))/(1+a_E^2));
<a name="line_84"></a>theta_E[3] := arctan(sqrt(1-2*a_E^2));
<a name="line_85"></a>theta_E[4] := arctan(sqrt((1-2*a_E^2)/(1+2*a_E^2)/3));
<a name="line_86"></a>
<a name="line_87"></a><span style="color:red">#@ v_on_c_E 
</span><a name="line_88"></a>v_on_c_E := table():
<a name="line_89"></a>
<a name="line_90"></a>for i from 0 to num_vertices_E-1 do
<a name="line_91"></a> for j from 0 to num_curves_E-1 do
<a name="line_92"></a>  v_on_c_E[i,j] := NULL;
<a name="line_93"></a> od:
<a name="line_94"></a>od:
<a name="line_95"></a>
<a name="line_96"></a># The values below could be computed by the function find_v_on_c_E
<a name="line_97"></a>
<a name="line_98"></a>v_on_c_E[ 0, 1] := 0;
<a name="line_99"></a>v_on_c_E[ 0, 2] := 0;
<a name="line_100"></a>v_on_c_E[ 0, 5] := 0;
<a name="line_101"></a>v_on_c_E[ 0, 6] := 0;
<a name="line_102"></a>v_on_c_E[ 1, 1] := Pi;
<a name="line_103"></a>v_on_c_E[ 1, 2] := Pi;
<a name="line_104"></a>v_on_c_E[ 1, 7] := 0;
<a name="line_105"></a>v_on_c_E[ 1, 8] := 0;
<a name="line_106"></a>v_on_c_E[ 2, 0] := 0;
<a name="line_107"></a>v_on_c_E[ 2, 4] := -1/2*Pi;
<a name="line_108"></a>v_on_c_E[ 3, 0] :=  1/2*Pi;
<a name="line_109"></a>v_on_c_E[ 3, 3] :=  1/2*Pi;
<a name="line_110"></a>v_on_c_E[ 4, 0] :=      Pi;
<a name="line_111"></a>v_on_c_E[ 4, 4] :=  1/2*Pi;
<a name="line_112"></a>v_on_c_E[ 5, 0] := -1/2*Pi;
<a name="line_113"></a>v_on_c_E[ 5, 3] := -1/2*Pi;
<a name="line_114"></a>v_on_c_E[ 6, 0] :=  1/4*Pi;
<a name="line_115"></a>v_on_c_E[ 6, 1] :=  1/2*Pi;
<a name="line_116"></a>v_on_c_E[ 6,13] := 0;
<a name="line_117"></a>v_on_c_E[ 6,16] := 0;
<a name="line_118"></a>v_on_c_E[ 7, 0] :=  3/4*Pi;
<a name="line_119"></a>v_on_c_E[ 7, 2] :=  1/2*Pi;
<a name="line_120"></a>v_on_c_E[ 7,14] := 0;
<a name="line_121"></a>v_on_c_E[ 7,15] :=      Pi;
<a name="line_122"></a>v_on_c_E[ 8, 0] := -3/4*Pi;
<a name="line_123"></a>v_on_c_E[ 8, 1] := -1/2*Pi;
<a name="line_124"></a>v_on_c_E[ 8,13] :=      Pi;
<a name="line_125"></a>v_on_c_E[ 8,16] :=      Pi;
<a name="line_126"></a>v_on_c_E[ 9, 0] := -1/4*Pi;
<a name="line_127"></a>v_on_c_E[ 9, 2] := -1/2*Pi;
<a name="line_128"></a>v_on_c_E[ 9,14] :=      Pi;
<a name="line_129"></a>v_on_c_E[ 9,15] := 0;
<a name="line_130"></a>v_on_c_E[10, 4] := 0;
<a name="line_131"></a>v_on_c_E[10, 6] :=      Pi;
<a name="line_132"></a>v_on_c_E[10, 9] :=      Pi;
<a name="line_133"></a>v_on_c_E[10,10] := 0;
<a name="line_134"></a>v_on_c_E[10,14] :=  1/2*Pi;
<a name="line_135"></a>v_on_c_E[10,16] :=  1/2*Pi;
<a name="line_136"></a>v_on_c_E[11, 3] := 0;
<a name="line_137"></a>v_on_c_E[11, 5] :=      Pi;
<a name="line_138"></a>v_on_c_E[11, 9] := 0;
<a name="line_139"></a>v_on_c_E[11,10] :=      Pi;
<a name="line_140"></a>v_on_c_E[11,13] :=  1/2*Pi;
<a name="line_141"></a>v_on_c_E[11,15] :=  1/2*Pi;
<a name="line_142"></a>v_on_c_E[12, 4] :=      Pi;
<a name="line_143"></a>v_on_c_E[12, 8] :=      Pi;
<a name="line_144"></a>v_on_c_E[12,11] :=      Pi;
<a name="line_145"></a>v_on_c_E[12,12] := 0;
<a name="line_146"></a>v_on_c_E[12,13] := -1/2*Pi;
<a name="line_147"></a>v_on_c_E[12,15] := -1/2*Pi;
<a name="line_148"></a>v_on_c_E[13, 3] :=      Pi;
<a name="line_149"></a>v_on_c_E[13, 7] :=      Pi;
<a name="line_150"></a>v_on_c_E[13,11] := 0;
<a name="line_151"></a>v_on_c_E[13,12] :=      Pi;
<a name="line_152"></a>v_on_c_E[13,14] := -1/2*Pi;
<a name="line_153"></a>v_on_c_E[13,16] := -1/2*Pi;
<a name="line_154"></a>v_on_c_E[14, 1] :=  theta_E[1];
<a name="line_155"></a>v_on_c_E[14, 9] :=  1/2*Pi;
<a name="line_156"></a>v_on_c_E[15, 2] :=  theta_E[1];
<a name="line_157"></a>v_on_c_E[15,10] :=  1/2*Pi;
<a name="line_158"></a>v_on_c_E[16, 1] := -theta_E[1];
<a name="line_159"></a>v_on_c_E[16, 9] := -1/2*Pi;
<a name="line_160"></a>v_on_c_E[17, 2] := -theta_E[1];
<a name="line_161"></a>v_on_c_E[17,10] := -1/2*Pi;
<a name="line_162"></a>v_on_c_E[18, 2] :=  theta_E[1]-Pi;
<a name="line_163"></a>v_on_c_E[18,11] :=  1/2*Pi;
<a name="line_164"></a>v_on_c_E[19, 1] := -theta_E[1]+Pi;
<a name="line_165"></a>v_on_c_E[19,12] :=  1/2*Pi;
<a name="line_166"></a>v_on_c_E[20, 2] := -theta_E[1]+Pi;
<a name="line_167"></a>v_on_c_E[20,11] := -1/2*Pi;
<a name="line_168"></a>v_on_c_E[21, 1] :=  theta_E[1]-Pi;
<a name="line_169"></a>v_on_c_E[21,12] := -1/2*Pi;
<a name="line_170"></a>v_on_c_E[22, 5] := -theta_E[2]+Pi;
<a name="line_171"></a>v_on_c_E[23, 6] := -theta_E[2]+Pi;
<a name="line_172"></a>v_on_c_E[24, 5] :=  theta_E[2]-Pi;
<a name="line_173"></a>v_on_c_E[25, 6] :=  theta_E[2]-Pi;
<a name="line_174"></a>v_on_c_E[26, 7] := -theta_E[2]+Pi;
<a name="line_175"></a>v_on_c_E[27, 8] := -theta_E[2]+Pi;
<a name="line_176"></a>v_on_c_E[28, 7] :=  theta_E[2]-Pi;
<a name="line_177"></a>v_on_c_E[29, 8] :=  theta_E[2]-Pi;
<a name="line_178"></a>v_on_c_E[30, 9] := -theta_E[3]+Pi;
<a name="line_179"></a>v_on_c_E[31,10] := -theta_E[3]+Pi;
<a name="line_180"></a>v_on_c_E[32, 9] :=  theta_E[3]-Pi;
<a name="line_181"></a>v_on_c_E[33,10] :=  theta_E[3]-Pi;
<a name="line_182"></a>v_on_c_E[34,11] := -theta_E[3]+Pi;
<a name="line_183"></a>v_on_c_E[35,12] := -theta_E[3]+Pi;
<a name="line_184"></a>v_on_c_E[36,11] :=  theta_E[3]-Pi;
<a name="line_185"></a>v_on_c_E[37,12] :=  theta_E[3]-Pi;
<a name="line_186"></a>v_on_c_E[38,10] := -theta_E[3];
<a name="line_187"></a>v_on_c_E[39, 9] :=  theta_E[3];
<a name="line_188"></a>v_on_c_E[40,10] :=  theta_E[3];
<a name="line_189"></a>v_on_c_E[41, 9] := -theta_E[3];
<a name="line_190"></a>v_on_c_E[42,12] :=  theta_E[3];
<a name="line_191"></a>v_on_c_E[43,11] := -theta_E[3];
<a name="line_192"></a>v_on_c_E[44,12] := -theta_E[3];
<a name="line_193"></a>v_on_c_E[45,11] :=  theta_E[3];
<a name="line_194"></a>
<a name="line_195"></a><span style="color:red">#@ v_on_c_cayley
</span><a name="line_196"></a>for i from 0 to num_vertices_E-1 do
<a name="line_197"></a> for j from 1 to 4 do
<a name="line_198"></a>  v_on_c_cayley[i,j] := NULL;
<a name="line_199"></a> od:
<a name="line_200"></a>od:
<a name="line_201"></a>
<a name="line_202"></a>v_on_c_cayley[ 2,2] := 0;
<a name="line_203"></a>v_on_c_cayley[ 2,4] := Pi;
<a name="line_204"></a>v_on_c_cayley[ 3,1] := Pi;
<a name="line_205"></a>v_on_c_cayley[ 3,3] := 0;
<a name="line_206"></a>v_on_c_cayley[ 4,2] := Pi;
<a name="line_207"></a>v_on_c_cayley[ 4,4] := 0;
<a name="line_208"></a>v_on_c_cayley[ 5,1] := 0;
<a name="line_209"></a>v_on_c_cayley[ 5,3] := Pi;
<a name="line_210"></a>v_on_c_cayley[22,3] := -1/2*Pi;
<a name="line_211"></a>v_on_c_cayley[23,4] := -1/2*Pi;
<a name="line_212"></a>v_on_c_cayley[24,1] := -1/2*Pi;
<a name="line_213"></a>v_on_c_cayley[25,2] := -1/2*Pi;
<a name="line_214"></a>v_on_c_cayley[26,1] :=  1/2*Pi;
<a name="line_215"></a>v_on_c_cayley[27,2] :=  1/2*Pi;
<a name="line_216"></a>v_on_c_cayley[28,3] :=  1/2*Pi;
<a name="line_217"></a>v_on_c_cayley[29,4] :=  1/2*Pi;
<a name="line_218"></a>v_on_c_cayley[30,4] := -1/2*Pi-theta_E[4];
<a name="line_219"></a>v_on_c_cayley[31,1] := -1/2*Pi-theta_E[4];
<a name="line_220"></a>v_on_c_cayley[32,2] := -1/2*Pi-theta_E[4];
<a name="line_221"></a>v_on_c_cayley[33,3] := -1/2*Pi-theta_E[4];
<a name="line_222"></a>v_on_c_cayley[34,4] :=  1/2*Pi+theta_E[4];
<a name="line_223"></a>v_on_c_cayley[35,1] :=  1/2*Pi+theta_E[4];
<a name="line_224"></a>v_on_c_cayley[36,2] :=  1/2*Pi+theta_E[4];
<a name="line_225"></a>v_on_c_cayley[37,3] :=  1/2*Pi+theta_E[4];
<a name="line_226"></a>v_on_c_cayley[38,2] := -1/2*Pi+theta_E[4];
<a name="line_227"></a>v_on_c_cayley[39,3] := -1/2*Pi+theta_E[4];
<a name="line_228"></a>v_on_c_cayley[40,4] := -1/2*Pi+theta_E[4];
<a name="line_229"></a>v_on_c_cayley[41,1] := -1/2*Pi+theta_E[4];
<a name="line_230"></a>v_on_c_cayley[42,2] :=  1/2*Pi-theta_E[4];
<a name="line_231"></a>v_on_c_cayley[43,3] :=  1/2*Pi-theta_E[4];
<a name="line_232"></a>v_on_c_cayley[44,4] :=  1/2*Pi-theta_E[4];
<a name="line_233"></a>v_on_c_cayley[45,1] :=  1/2*Pi-theta_E[4];
<a name="line_234"></a>
<a name="line_235"></a><span style="color:red">#@ v_track_E
</span><a name="line_236"></a>for i from 0 to num_vertices_E-1 do 
<a name="line_237"></a> v_track_E[i] := []:
<a name="line_238"></a>od:
<a name="line_239"></a>
<a name="line_240"></a><span style="color:red">#@ c_track_E
</span><a name="line_241"></a>for j from 0 to num_curves_E-1 do
<a name="line_242"></a> c_track_E[j] := []:
<a name="line_243"></a>od:
<a name="line_244"></a>
<a name="line_245"></a>for i from 0 to num_vertices_E-1 do
<a name="line_246"></a> for j from 0 to num_curves_E-1 do
<a name="line_247"></a>  if v_on_c_E[i,j] <> NULL then
<a name="line_248"></a>   v_track_E[i] := [op(v_track_E[i]),j = v_on_c_E[i,j]];
<a name="line_249"></a>   c_track_E[j] := [op(c_track_E[j]),i = v_on_c_E[i,j]];
<a name="line_250"></a>  fi;
<a name="line_251"></a> od:
<a name="line_252"></a>od:
<a name="line_253"></a>
<a name="line_254"></a><span style="color:red">#@ find_v_on_c_E 
</span><a name="line_255"></a>find_v_on_c_E := proc()
<a name="line_256"></a> local i,j,k,t0,t1,x0,x1,theta0,recognise_theta;
<a name="line_257"></a> global v_on_c_E,v_on_c_E_string;
<a name="line_258"></a>
<a name="line_259"></a> for k from 1 to 3 do 
<a name="line_260"></a>  theta0[k] := evalf(subs(a_E=0.1,theta_E[k]));
<a name="line_261"></a> od;
<a name="line_262"></a>
<a name="line_263"></a> recognise_theta := proc(t)
<a name="line_264"></a>  local t0,i,k,e;
<a name="line_265"></a>  if type(t/Pi,rational) then return t; fi;
<a name="line_266"></a>  t0 := evalf(subs(a_E=0.1,t));
<a name="line_267"></a>  for k from 1 to 3 do
<a name="line_268"></a>   i := evalf(12*(t0 - theta0[k])/Pi);
<a name="line_269"></a>   e := i - round(i);
<a name="line_270"></a>   if abs(e) < 10.^(-20) then
<a name="line_271"></a>    return theta[k] + round(i)*Pi/12;
<a name="line_272"></a>   fi;
<a name="line_273"></a>   i := evalf(12*(t0 + theta0[k])/Pi);
<a name="line_274"></a>   e := i - round(i);
<a name="line_275"></a>   if abs(e) < 10.^(-20) then
<a name="line_276"></a>    return -theta[k] + round(i)*Pi/12;
<a name="line_277"></a>   fi;
<a name="line_278"></a>  od:
<a name="line_279"></a>  return t;
<a name="line_280"></a> end:
<a name="line_281"></a>
<a name="line_282"></a> v_on_c_E := table():
<a name="line_283"></a>
<a name="line_284"></a> for i from 0 to num_vertices_E-1 do
<a name="line_285"></a>  for j from 0 to num_curves_E-1 do
<a name="line_286"></a>   v_on_c_E[i,j] := NULL;
<a name="line_287"></a>   try
<a name="line_288"></a>    t0 := simplify_E(c_param_E[j](v_E[i]));
<a name="line_289"></a>    t1 := Re(evalf(subs(a_E=0.1,t0)));
<a name="line_290"></a>    x0 := evalf(subs(a_E=0.1,v_E[i]));
<a name="line_291"></a>    x1 := evalf(subs(a_E=0.1,c_E[j](t1)));
<a name="line_292"></a>    if d4f(x0,x1) < 10.^(-20) then
<a name="line_293"></a>     v_on_c_E[i,j] := recognise_theta(t0);
<a name="line_294"></a>    fi;
<a name="line_295"></a>   catch:
<a name="line_296"></a>    v_on_c_E[i,j] := v_on_c[i,j];
<a name="line_297"></a>   end try:
<a name="line_298"></a>  od;
<a name="line_299"></a> od;
<a name="line_300"></a>
<a name="line_301"></a> v_on_c_E_string := "";
<a name="line_302"></a>
<a name="line_303"></a> for i from 0 to num_vertices_E-1 do
<a name="line_304"></a>  for j from 0 to num_curves_E-1 do
<a name="line_305"></a>   if v_on_c_E[i,j] <> NULL then
<a name="line_306"></a>    t0 := v_on_c_E[i,j];
<a name="line_307"></a>    t1 := sprintf("%A",subs(theta='theta_E',t0));
<a name="line_308"></a>    if t0 <> 0 and type(t0/Pi,rational) then 
<a name="line_309"></a>     while length(t1) < 7 do t1 := cat(" ",t1); od;
<a name="line_310"></a>    fi;
<a name="line_311"></a>    if substring(t1,1..7) = "theta_E" then t1 := cat(" ",t1); fi;
<a name="line_312"></a>    v_on_c_E_string := cat(v_on_c_E_string,sprintf("v_on_c_E[%2d,%2d] := %s;\n",i,j,t1));
<a name="line_313"></a>   fi;
<a name="line_314"></a>  od:
<a name="line_315"></a> od:
<a name="line_316"></a>end:
  </pre>
 </body>
</html>
    