(game-module "magnusvew"
  (title "Panzers at Magnusvew")
  (blurb "The Germans try to hold back the Russians at Magnusvew")
  (base-module "panzer")
  (variants (see-all true))
  )

(set terrain-seen true)

(set last-turn 10)

(table independent-density (u* t* 0))

;;; need to make a point-counting scorer.

(add u* point-value 1)
(add minefield point-value 0)
(add town point-value 10)

(add u* start-with 0)

(set advantage-max 1)

(area 60 40)

(area (terrain
  "41f19a"
  "22fc19f18a"
  "23fc19f17a"
  "4f2e4fefe10fc11fd8f16a"
  "2fc3f2e2fe2fe6fd2fc2d10f2d7f15a"
  "3f2c5fe3fe6fdfc4d2f3e4f5db3f14a"
  "6f2b2a2fe8f2df3cd2f5e3f5db3f13a"
  "7f2b2a2fe11fcdcfdf6e3f4db3f12a"
  "8f2b2a10fd3fcfcfd2f2d3e4fdfbd3f11a"
  "9fb2a11fd2fc2fcf5d4e2fdf2b2d2f10a"
  "9f2bab10fd3e2df2c4d2f4ed2f3d3f9a"
  "6fd3f3b10fdef2e2dcfc2f2d3f2e2fd2f2d3f8a"
  "4fe7fb10f2e2f2edfcf2c2fd5f3d2fdb3f7a"
  "23fed3f2ed4fc2d2f3edfdf3d4f6a"
  "6fe16fed4f2e5f2cd3f2e2fdfdfd4f5a"
  "6fe6fc10f2e4f2e6fc4fed2f3dfd4f4a"
  "5fe8fc9f2e5fe7fcd3fd3fd8f3a"
  "5fe8fc10f2e4f2e3fd3fcd3f5d8f2a"
  "6fe7fc10f2e4f3e2f3dfcf7df2d7fa"
  "6fe8fc10fe5f2e3f3dfc2df2e3f5d5f"
  "a8fe6fc9f2efd2f3e2fdfdfc8fc5d4f"
  "2a5fe9f2c7f2ef2dfdefd3fd10fc3f2d4f"
  "3a5fefe8f3c4f2ed2f2d2ed2fd2fd6fefcf2dfd4f"
  "4a5f2e11f3c2fe5de2df2d2f3d5fefc4d4f"
  "5a20fc2f2e2d2ed3f3df3d4fefc2f3d3f"
  "6a15fc7f5e3f9d2f2efcf3d4f"
  "7a15f2c4f2d7f3df3d3f2d2fcf2d5f"
  "8a16fc4fd5fdf4d3f2d3fdfcf2d5f"
  "9a5fe10fc3df3dfd3f3d2e2fd2fd2fcf3d4f"
  "10a5fe11fd2fd2f4d2fdf2e3fd7f2d4f"
  "11a5fe5fe6d8fd4f2e10f3d3f"
  "12a5fe4f2ef2d10fd6f4e3fe3f2d3f"
  "13a5fe3f3e3fe9fd7f7e7f"
  "14a9f4e3f2e6fd12f2e7f"
  "15a8fd6efdf4e2f2d4f5d5fd4f"
  "16a8f2df4e3df3ef4d2f5d5fd4f"
  "17a11f5e3dfedf4d2f6d3fd4f"
  "18a14f4e6f2d6f5d5f"
  "19a33f3d5f"
  "20a40f"
  ))

(area (aux-terrain stream
  "60a"
  "22ae9ai27a"
  "23aD8aj27a"
  "cu21aj8aj27a"
  "2a@20aj5ak2sv27a"
  "3a>su17af5ab3a>u25a"
  "6a>sq15aBsu8a<24a"
  "25a@a@32a"
  "26aDa@31a"
  "26ab2a@13acu15a"
  "11ai18aFu13a>u13a"
  "11af18afa@14a>ukq9a"
  "12a@18aDa>u14aE10a"
  "13aD17ab3a@13aj10a"
  "13aj22a>u11af10a"
  "13af24a@11a@9a"
  "14aD24a@11aD8a"
  "14aj25aD10aj8a"
  "14af25ahq8akr8a"
  "15a@25aD8aj9a"
  "16a@24af8aj9a"
  "17a>u23a<7aj9a"
  "19aBsu28af9a"
  "20aDa>su26aD8a"
  "20af4a>sq23aj8a"
  "21a@29aj8a"
  "22a>u20acsu4aj8a"
  "24a@22a>u2aj8a"
  "25a@23a>sv8a"
  "26a<25aD7a"
  "52af7a"
  "53aD6a"
  "53aj6a"
  "53aj6a"
  "51aksr6a"
  "50acr8a"
  "60a"
  "60a"
  "60a"
  "60a"
))

(area (aux-terrain road
  "60a"
  "11ai7ae14ae25a"
  "11aj8aD14aD24a"
  "11an8af14aj24a"
  "11aj@8a@13af24a"
  "11aja@8a@13a@23a"
  "11af2a>u7aD13a@22a"
  "12a@3a@6af14a>u20a"
  "13a@3a>u5aD15a@19a"
  "14aD4a>u3af16a@18a"
  "14aj6a@3aD16aD17a"
  "14aj7a@2af16af17a"
  "14aj8a>uaD16a@16a"
  "14aj10a>v17a@6akq7a"
  "14aj12a@17a@5aj8a"
  "14af13aD17a@3akr8a"
  "15aD11akvaksu2aku8aksR2sr9a"
  "15aj8ak2sraFr2a>sra>u2ak3sr2a@11a"
  "15af7akr4aj9a>sr7a@10a"
  "c3su11a@4aksr5af20a@9a"
  "5a>su9aF3sr8a@20a@8a"
  "8a>2su3aksr13aD20a>u6a"
  "12a>2sr15aj22a@5a"
  "31aj23a@4a"
  "31af24a@3a"
  "32a@24a@2a"
  "33a@24a<a"
  "34a@25a"
  "35aD24a"
  "35aj24a"
  "35af24a"
  "36a@23a"
  "37aD22a"
  "37aj22a"
  "37af22a"
  "38a@21a"
  "39aD20a"
  "39aj20a"
  "39ab20a"
  "60a"
))

(side (name "Russia") (noun "Russian") (adjective "Russian") (class "russian")
  (color "red") (emblem-name "soviet-star"))

(side (name "Germany") (noun "German") (adjective "German") (class "german")
  (color "gray") (emblem-name "german-cross"))

(MG 52 24 1)
(r45 52 25 1)
(r45 52 25 1)
(r45 50 26 1)
(r76at 56 16 1)
(r76at 45 24 1)
(r76at 46 22 1)
(r82m 50 27 1)
(r82m 47 22 1)
(r82m 53 19 1)
(r82 53 17 1)
(r120 55 17 1)
(r120 54 16 1)
(rengrs 45 21 1)
(rrecon 49 22 1)
(rrecon 50 22 1)
(rrecon 48 24 1)
(rrifle 40 20 1)
(rrifle 39 22 1)
(rrifle 46 22 1)
(rrifle 54 18 1)
(rrifle 41 23 1)
(rrifle 42 21 1)
(rrifle 44 23 1)
(rrifle 51 27 1)
(rrifle 44 23 1)
(rsmg 52 19 1)
(rsmg 52 17 1)
(rsmg 44 24 1)
(rsmg 44 24 1)
(rtruck 47 23 1)
(rtruck 51 25 1)
(rtruck 52 26 1)
(rtruck 47 23 1)
(rtruck 51 26 1)
(rtruck 51 26 1)
(rtruck 56 15 1)
(rtruck 52 26 1)
(rtruck 51 25 1)
(rtruck 46 23 1)
(rtruck 54 17 1)
(rtruck 46 23 1)
(rtruck 55 16 1)
(rtruck 54 17 1)
(rtruck 53 18 1)
(rtruck 53 18 1)
(rtruck 55 16 1)
(rhalf 45 23 1)
(rhalf 45 23 1)
(rhalf 52 18 1)
(rhalf 52 18 1)
(rhalf 45 22 1)
(rhalf 45 22 1)
(SU-152 43 22 1)
(SU-152 43 22 1)
(SU-85 40 21 1)
(SU-85 40 21 1)
(t34 51 24 1)
(t34 51 24 1)
(t34 50 24 1)
(t34 50 24 1)
(t34 50 23 1)
(t34 44 22 1)
(t34 48 23 1)
(t34 48 23 1)
(t34 49 23 1)
(t34 49 23 1)
(t34 50 23 1)
(t34 44 22 1)
(t34/85 42 22 1)
(t34/85 42 22 1)
(t34/85 41 21 1)
(t34/85 41 21 1)
(t34/85 41 22 1)
(t34/85 41 22 1)

(town 28 23 2 (n "Magnusvew"))
(town 17 19 2)
(g75 28 26 2)
(g75 30 24 2)
(g20 33 20 2)
(g20 30 25 2)
(g20q 33 19 2)
(g81 31 19 2)
(g120 27 27 2)
(grifle 24 20 2)
(grifle 25 20 2)
(grifle 20 20 2)
(grifle 40 4 2)
(grifle 41 3 2)
(grifle 39 5 2)
(grifle 17 32 2)
(grifle 18 32 2)
(grifle 19 31 2)
(grifle 29 23 2)
(grifle 28 25 2)
(grifle 27 26 2)
(grifle 34 10 2)
(grifle 34 11 2)
(grifle 30 20 2)
(gsmg 19 20 2)
(gsmg 18 20 2)
(gsmg 17 20 2)
(gtruck 38 4 2)
(gtruck 39 3 2)
(gtruck 37 5 2)
(gtruck 19 19 2)
(gtruck 20 19 2)
(gtruck 21 19 2)
(gtruck 18 19 2)
(gtruck 29 21 2)
(gtruck 22 28 2)
(gtruck 21 29 2)
(ghalf 32 17 2)
(ghalf 25 27 2)
(ghalf 29 24 2)
(ghalf 30 18 2)
(ghalf 35 10 2)
(ghalf 35 11 2)
(ghalf 17 31 2)
(ghalf 25 27 2)
(ghalf 30 18 2)
(ghalf 19 30 2)
(ghalf 18 31 2)
(Maultier 26 29 2)
(Wespe 26 17 2)
(Hummel 36 5 2)
(gw38 31 18 2)
(marder3 33 14 2)
(marder3 26 26 2)
(stug3 33 13 2)
(stug3 34 12 2)
(pz4 22 21 2)
(pz4 21 21 2)
(pz4 22 22 2)
(pz4 23 23 2)
(pz4 23 22 2)
(pz4 29 20 2)
(Panther 27 23 2)
(Panther 26 24 2)
(Panther 27 22 2)
(Panther 27 21 2)

(game-module (instructions (
  "Points are for destruction of the enemy.  Towns are worth 10, all others 1."
  ""
  "The game is limited to 10 turns, so time is of "
  "the essence."
)))

(game-module (notes (
  "This is a re-creation of a battle in Poland."
)))

(game-module (design-notes
  "This is based on a design by Robert Harmon published in the General."
))
