Problem Format
==================

1                  -- シルエットは1つのポリゴン
4                  -- 4つの頂点(CCW)
0,0                   -- 座標0
1,0                   -- 座標1
1/2,1/2               -- 座標3
0,1/2                 -- 座標4
5                  -- 5本の辺(CCW?)
0,0 1,0               -- 辺0
1,0 1/2,1/2           -- 辺1
1/2,1/2 0,1/2         -- 辺2
0,1/2 0,0             -- 辺3
0,0 1/2,1/2           -- 辺4

Solution Format
==================

7                   -- 折り目がつく頂点7つ
0,0                 -- 座標0
1,0                 -- 座標1
1,1                 -- 座標2
0,1                 -- 座標3
0,1/2               -- 座標4
1/2,1/2             -- 座標5
1/2,1               -- 座標6
4                   -- 小面は4つ
4 0 1 5 4           -- 4頂点で座標0->1->5->4で構成
4 1 2 6 5           -- 4頂点で座標1->2->6->5で構成
3 4 5 3             -- 3頂点で座標4->5->3で構成
3 5 6 3             -- 3頂点で座標5->6->3で構成
0,0                 -- 座標0は(0,0)に重なる
1,0                 -- 座標1は座標(1,0)に重なる
0,0                 -- 座標2は座標(0,0)に重なる
0,0                 -- 座標3は座標(0,0)に重なる
0,1/2               -- 座標4は座標(0,1/2)に重なる
1/2,1/2             -- 座標5は座標(1/2,1/2)に重なる
0,1/2               -- 座標6は座標(0,1/2)に重なる