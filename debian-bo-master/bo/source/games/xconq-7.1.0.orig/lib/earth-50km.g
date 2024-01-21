(game-module "earth-50km"
  (title "Earth 800x320")
  (blurb "Earth's terrain, at 50km/hex")
  (default-base-module "standard")
  (variants
   (see-all false)
   ;; The following is useful for hacking on the terrain
   (bare true
    (true
    (set synthesis-methods nil)
    (add u* start-with 0)
    (add city start-with 1)
    (add u* independent-near-start 0)
    (table independent-density (u* t* 0))
    ))
   )
  )

(world 800)

(area 800 320)

(area (terrain
  "800h"
  "125h4a580h3a45h10a5h2a10ha15h"
  "72ha52h6a3ha573ha2h29a4ha12h17a11h2a13h"
  "125h7a574ha3h30a17h17a26h"
  "126h11a571h32a17h17a26h"
  "26h2a78haha18h10a174ha396h33a5ha11h17a25h"
  "23h4a42ha36h4a19h9a64ha29h2a44ha8h7a6h13a365ha3h4a13h2a10h31a4h2a11h16a25h"
  "23h4a78h5a20h8a61h5a5h2a8h3a7h9a7h8a12h3a8h47a4ha87ha11ha241ha6ha6h6a4h3a4h3f3e2h68a24h"
  "23h3a79h5a21h6a38hahaha18h25a5h12a5h9a4h2a5h67a41ha7h6a20h2a5h3a5ha4h3a8ha225h5a6h21a6f6e2h67a23h"
  "7ha15h3a18ha61h3a2ha21h6a36h7ah2a8h31a4h13a4h16a3h70a22h8a7h17a14h8a4h4a4h11a5h2a188ha14ha19h31a9f2e3f71a13h7ah"
  "8a14h3a50ha30h4a2ha21h5a36h12a4h150a5h5a5h33a13h33a4f7a175h5a4hah3a4ha26h22a3fa11f3ef5ea10e4a6fa5f41a11h9a"
  "9a9h7a21hf41ha17h6a23h5a35h114a4f74a3fa3f11a10f2h19a8f10a8f5a17h7f150h11a3h4a4ha2ha2ha14h26a2f2a2fafef2efe2f23efe3faf3af46a3h11a"
  "10a6h9af59h3a18h8a21h6a33h112a20f38a17fa14f2a22f3a9fa30f5a15h11f2ha75ha9h3a4ha22ha7h8a9ha3h68a3f2a5f3efefef4ea16e2fe7fa3f58a"
  "25afa20haf3ha35ha16hah7aha20h5a31h114a2f4g18f27a5f2a100f3a9f3h18f30h9f3h9f25ha5h7a3h3a14h10a3h10a8h46a7f2a4f7a2fa4fa8fefefef4efefa2ea16ef9e4f54a"
  "26af13hf5h4a2fhfa12h3a37h8a3h3a16h4a31h114af7g18f23a144fhaha17h30f15h2a7h2ah17a4ha7h50a7f16a18f6a20f6e4fe4f13e4f4e4fef4e51a"
  "26a4f5h3a3hfa2h8a13h5ah2a4ha4hf17ha2h2a2h14a16h4a30h113a2f2g2f2g21f14a5f2a148f2h4a9h34f21h59a14f6a46f5a12f6e4f4efe4f2ef4e6f5efef10efe5f47a"
  "34a3h2a3hf3h10a18h2ah19a3ha7h16a17ha2ha27h114a2fgf3g24f12a6fa149f6a6h39f7h2a2h2a5haha4h41a77f5a14fe2f2ef4ef2e2f10e2fefe2f10ef3efe2fa6e3f43a"
  "36a2h4af2hf13a2f3af6ahahah23a2ha9h13a16h4a27h114afg30f11a157f9ah42f2hahah9f11a5f33a77f6a15fe7f4e9f2e6f3e7f2ef3e3f4efefef4efa4f34a"
  "44af2a3f11a4f17af18ah3ah19a15h5a27h113a3f2g12fa2f4a8f11a163f3ah30fh12fh4a11fh3a14f29a77f7a16f2efef3efe24fe2f7e5fe2fefe2fefe3f2ef2eaea3e25a"
  "ag41a2f2afaf34af17a4h21a13h6a26h112a2f4g12f7a9f11a211fa34f26a75f10a16f2e2f2e7f2e2fe2fe21f4e7fe4f7efe2f3e2f2a7f3ea4efe5f"
  "fg41af3afa2f34a2f41a12h6aha24h111a3f3g11f9ae9f10a247f18a6f2a73f12a19fe5f5e9fe20fefef9efe5f3ef6efefef3e7f3e5f"
  "3f2g28a2f8a7f39afa6f33a11h6ah2a20h113a3f3g11f3e7ae8f9a250fa5f7a82f18a21f5e2f2e5ge35fef6e2f2ef4ef3efef12e2f"
  "4fg23a5f2a10f2a3f38a5fa5f31a12h6a2h4a17h112a2f2gfg11f4e7a2e7f8a339f27a16f9efhg3h3ge40fe2f2ef6efef2ef2efe2f2ef2e2f"
  "e3fg21afaf2a19f38ae2fa9f30aha8hah7a22h111a4f2g12f3e9ae7f7a296fa42f28a4f2a9f6e2fe3g2hgh4gh7g26fa7fe9f3e2fe3fefe2fef2e"
  "eg5f4af9a4fa3f2a3f2afa5fa7f39ae3fa4f2a2faf27a11h14a15h111af3gf2g11f2e10aea7f5a298fa21f7a11f37a13f2e2f2g3h4gf6g2f3g25f4a6fefe6fa2f13e"
  "fa2fg3fa3f9af2a4f2a3f2a3fa3fafa5faf40a2faf3afa5f28a7h18a13h110a2f2g2fg12f3e10a2e8f3a286f2a16f2a11f59a15feg3h4gf6g3hg2f3g36fe9f4efe2f2ef"
  "4fafa3f5a5f4a14fa12fa2f39a2fa2f3a8f27ah2a2h20a12h109a2f2g2fg11f2ea2e10ae9f2a285f6a14f2a8f62a15f2gh3ge2f5g2e2g2hfh4gf9g26fef2e5f2e8f"
  "9fa2f8a29fa2f42a4fa2fa8f27a3h22a11h109af3gf2g11fea3e10afe2a7f2a284f7a14f2a6f63a13f2e2f2g2e2f4e5gfe2hg10hf8g27fef2e9f"
  "12f3af2afa12fa5fa14f42a4fa11f52a10h109af3g14f6e10afe292f10a13f3a3f65a29f4gf2g4hg8hgh4gf8g32f"
  "16fa5f4a20fa7f42a2fa5fa4fa2f52a9h35a6g69a3g14f6e11a2e5f3e7f2a62fg210f12a12f72a12fe3f3e2fe3f2e3fe9gf19g2e7g26f"
  "13fa2faf4a16f2a5f2a3f3a3f40a2fa15f51a8h34a8g68a3g12fafa2ea2e11a4e4fa3ef2e5a4f2a55fg209f16a8f74a10f2e3a2e4f4e4ae6f6g2f2g2fe2a19g11f2a5f5ga"
  "26fa30f39ag6fgfg4f3g2f8ag42a7h35a8g67ae2g14f7e2b8a2e2f6aef5e2a4e59fg212f11a6f78ae6f2g3fe2b4eb2a2fef3g7fea2efe2gf8g6h10g11f7g6f"
  "23fa2fa29faf38a2gfg7fg2fgfg3f7a2g42a6h36a7g68ae2g13f7e2b9ae9a2e2a2e2a4e269f14a6f79aeg4f2g2fea3bef2ba14f8a2efe2a6gf9h6g2f3g4f5gfg6f"
  "34f2a2f2a20f37ag5f2g2f2g4f3g3faf2a4g42a3g40a2e2g42a4e23a4f7a4f7e2b7a2b2e3b24e6fe45fefg181f36a10f82ae2f3e4a3feg4f7a5f16a3f11gh6g2f2g5fg5fgf2g2f"
  "34f2a2f2a7f2a11f37a2gfg5f3gf3gfgf2g5f6g40a3g40a2e2g42a5e24a2f7a4f7e2b7a2b2e3b31ea43f2e3fg72fe104f37a11f91a5f2g2f9a2fa2f22a3g2f3g4h3gf2g4fg7f3g2f"
  "3g40f2a18f35ag6f2g3f2g14f5g126a3e3ge28ae3a2e2f7e2b7a2b2f3b9e3f22e9f6e8f5e18fg68fe5fa97f41a14f56a2g29a5g17af26a2ga2fgf3h4g2fg10f4g2f"
  "gf2g39f2a8fa9f35a5gfg3f3g13fg2fgf3g125a3e4ge26a3e3ae2f7e2b7a2b2f3b9e4f37e7f6e10f2e2fe2fg18f3e2f3e29fef2e5f2e3fe2fe98f42a15f55a2g29a5g44a2g4a3g2h4g2fg10fgf2gf"
  "3f2g65f24af5a2g3fg24f3g127ae4g26a5e4a7e2b7a2b14e4f53e14feg11fe2fef6e2f5e14fe6fef6e2f4e3fefe2fe36f2g53f2e47a15f83a6fe6a3gfe35a3g2a4g2h3g4f2g11f"
  "g3f2g67f21a2f4a2g3fg25f4g125ae4ge25a6e4a7eb7a2b72e13f2eg4f9e2f6e2f6e2f2e10fe2f12ef5e2f2e3fe36f4g51f2e47a15f83a7f6a3g2f37ag2a2gf2g2h3g4fg5f2g4f"
  "3g6f14e21fa31f23a39f116a3e3a8e24a6e18a2b85e2f2gef11e2f19e6f24e2f3e37fg2ae53f2e4af40a15f79a5gfe58a4g2h4g13fg"
  "5g5f13e21fa21fa11f20a14fa25f116a4e2a9e23a7e17a2b85e2f2gf12e3f20e4f29e16fe19f2g2a2e53fe4a2f39a15f77a7g2f56a2fa5g2h3g11f2g"
  "3g2f2g4f17e16fa13fa21f18a14f2a24f2e113a5e2a11e21a8e16a2b86ef3g13e2f21e3f30e11fe3f2e18f2g2a2e53fe4a2f39a15f77a7g2f57a2fa6gh3g10f2g"
  "3f5g4f26e52f8a26fa19f109a7e5a7e22a2e9b2a2b7e2b26e2f59e2g2f31e3f37e13f3e18f2a2e56f44a14f8a2g3ag61a2g3e70a3g3h2g6f2e2f"
  "3f6g5f25e52f7a26fa4fa10fe4f108a7e5a8e21a2e9b2a2b8eb27e2f58e2gfgf31e2f38e6fe6f2e18f2a2e58f42a12f11ag3ag62ag3e69afag2a3h2g3f2g2efe"
  "e6fgf4g2f30e4fafa5f2a7fa25f6a21fa7f2a10fa3fe4f105a7e6a8e13a38ef82egef71e5f2g6f6e11fe3a64f3e3fa2b2f28a8f2e69a2g3a2g73a2f3a2f2a5g3f2g3e"
  "e6f4gf2g2f30e4fa6f2a7fa25f6a20fa21f2a7f105a6e7a7e13a38e2f81e2gef73e2fe3g7f4e10f4a65f2e4faba2f27a8fe70a2g3a2g80agf3a4g3fg2e"
  "9e5f2g2f31e4fa2fafa35f6a38f2a4fe5f104a6e5a11e9a22e4f94e4fg2f74e2fe5f2g4f3e3f4e6a3e10fe6fe51fababf27a7f33a2g25a2g3ag3ag86a2f3a2f2a7ge"
  "9e7fg3f29e10fa36f4a38f2a10f104a6e5a11e9a16ef6e4f93e4fg77e2fe4f2g6f7e6a5e28fe17fe21fabab2f27a6f35ag25ag3a2g2a2g85a3f3a2f3a6g"
  "g2f11e3f2gf29e6fa4fafa34f3a49f107a2e9a11e5a17e3f31e2f69e2f2ef77efe2fe2f6g3f4e5a9e4fefef2e18f2e7f3efe26faba2f29a2f50ag2ag4ag102a2f2a3f2a5g"
  "2fg2f9e2fgf2gf28e5fa5f2a15fa20f2a48f2a2e104a2e9a11e5a18e2f31e3f69e4fg76e7f6g4f3e4a12e2f4e2f3e15f2e3fefe3fe20fe7fa2baf29a2f49a2g3ag4ag107a2f2ah2eg"
  "2ef4g2f7e3fg3f31e4fa5fa13fa35fa17f2e9f2ef4a2e113a7e11a9e2f40e2f72ef67eg10e7f2g4f4gf26ef4e8f2e9f2e3f3e17f3e6fbab2a2f27a2f167afaef4a"
  "2aef3g3f6e6f2g30e4f2a3fa50f2a12f6e8f8a2e113a7e11a9e2f41ef75eg64e2g9e2f3g2f2gefefe3gf27e2f4e7fe8f2e3f4efe15f3e6fbab2a3f27af169afe2f2a"
  "2e2afgf2e2g7e2g3fgf31e2f3e2f2e3fa11f4ae36fe2f5e21a2efaf109a3e11a9e2f5e2f30eg85eg3e3d53e6g9e3f7g37e2f3efe6f2g10f4efefe3fe8fe9f2b3a3f200ag2f"
  "2efeafgfe3g6e2g3f2gf39e2f2a11f6ae38f3e8a4e10a2e3f119ae3a9e2f5e2f28e6g86e4d51e7g10e2f8g39e10f2g4f2e3f5efe14fe9f2b3a3f200aegf"
  "f3eae4f2g6e5f2g2f38e2f2a11f7a31f9e3a5e4a3e8a2e4fae117ae2a10ef5e2f23egege8g85e4d52e6g14e7g41e7f2g3f2e4f5efe6fe4f4e9f2b4a3f198afeaf"
  "5a2befg2eg7e5fg2f46e2f3e9a3e27f8e3a2e5fe4a2e7a2e6fae111a48e3ge3g4e5g81e6d51e6g19e4g39e6f2g3f4e2f6efe10f5e8fe6a2f202a"
  "4a2f2befge2d6e5f2g2f45e2f2e11a3e26f7e3a2e6fe12a4e5f3e111a29e5g3e5g9e2g7e4g79e8d50e7g19e4g40e4f2g3f5ef5e2fe10f6e7f2e5a2f202a"
  "5a2fbefge3de3d2e6f2gf42e2f4e5a2f6a4e21f7e3a3e6f2e12a14e109a29e15g18e4g56e8d14e5d53e4g65e4f2gfa12e2fe2fe7f4e9f7a2f20af181a"
  "5a3fbefg8d3e4f2gef41e2f4e5a2f7a6e18f4e4a2e10f13a2e3a9e113a23e13ge4g18e4g55e9d14e5d33ea18e4g66e3fg3f12e2fe9fef3e9f8af21af180a"
  "6a3fefge2de3ded4e2g2fg2f46e2a12f3ae2a3e13f4e3a5e10f2eae20aeae114a21e6g6e2g2e4g18e5g13e3ae2a5b35e3d2e7de2a4e2d2e4d21e8a2d16e8g18e4ge5d19e8d8e4f14e2f2e8f5e7fe10af201a"
  "6ae2fe2fge9d2e2g2f3gf47e12f2ae2ae2a3e11f4ea6e11f3e5ae2ae130a21e4g11e4g19e5g12e3a2ea6b28e5be3d2e7d4a7e4d20e2a7e2d16e9g16e6g6d10e2d2e3de11d6e2f19e9f4e7fe212a"
  "7a2fe2fg2e9d3e5g3f43e5f7e3a4ea4b5e3f6e3a6e9f3ea2ef2a2fe133a7e2f8eg2e4g9eb3e5g11e10g9e5a5e2a3b25e7a2be11d5a7e7d5e8d3e2ae6d6g7e11g16e4g12d3e29d2e2f18e2fe5f2ef3e7f11ae17a2f182a"
  "7a2f2e2fge10d3e4g2fef42e5f6e4a5e5b13ea8e9f2e2a2e5f2e133a6e3f6e3gegeg10e2b4e4g10e10g8e6a7ea2b25e7a2be11de5a8e6d4e10d2e2a7d6g7e10g16e4g24de4de14d22e2fe8f3e7f11a2e17af182a"
  "9afe2fege3de6d3e3g6f51e4a5e3ae2a12ea8e5f5e3a5f2e136a7e3f5e2g2eg11e4a3e6g11e2g11e9a2e4a27e7a4e10d2e4a8e8d2e19d11g6e25gd2g18de8de16d23e7f8e3f2e12a2e11a2faf184a"
  "8a2fe3fg2e11d2e2g4f2gf51e3a5e4a13e2a8e2f7e4a5f138a8e3f4e2g2e2g11e3ab3e6g5e2g4e4g8e10ae3ab27e6a5e10d2e5a8e5ded2e7de2d2e4de2d12g5e24g3de43d25e6f7e4fe13aef2e3ae6a2f185a"
  "9af2e2f2ge7de5d2e4f3g2f4eg44e3a5eae3a12ea10ef5e6a4f140a8e2f5eg2e3g11e2a2ba2e4ge2g2e2g17e14a2ba26e6a3e11d3e4a8e5ded3e6d2e9de12g5e24g3de20de6de7de4d28e6f7e4f14ae3f3e8af185a"
  "9a2fe4fe14d4e3gf2gf49e3a7e2a8eb4a4e3f7e8a3e126a7e3g3e3fg16e9a6e5a4e5g3e2g2e2g11e21a5e3g14e8a4e10d2e2a3e5d2e5ded2e6g3e7d7e14gh10g50d3ed28e5f8e2f3e14aef2gf3e193a"
  "10a2fe3f2e16ded2e2f2gf2eg45e3a7e2a7e6a4e3f6e10a2e126a6e6g2e2fe3g13e10a7ea2b3a2e6ge2g2e2g11e23a2e6g13e8a4e10d6e6d2e4d2ed2e6g17e13g2h8g51d32e6f10e14a4e3fe195a"
  "11a4fgf2e13dg5de4g2f2e3g42e3a7ea26e138a5e7g8e4g3e12a2e4a7e6ae9g5e7g2e25a2e12g6e9a2e9de18d2ed2e4gf9g4e3a6e6g6h6ge51d31e8f4e20a7e195a"
  "11ae5f2e3de6d2e2dg3dbe5gf2e4g41e4a6ea4e6a16e139a4e6g12e2g2e13ae6a7e4a2b2e7g2e10g2e26a3e13g3e8a2e9de8d2e8de2d5e2f10g8eg3e5g7h6ge44de7d30e8f3e21aeae3ae195a"
  "12ae4fge19dbe2g3f4e3g41e3a8e8a19e137aeg22ege22a8e3ab3e7g2e5g2eg2e8a8e11a11e5g3e8ae2a11d2e13de2d7e4f7g4e5g2e5g6h4g3e45d2e6d31e5f4e23ae200a"
  "13a3fef2e18d2beg4f3e3g42e2a7e6a17e143a3g17eg4e13a2e8a8e2a2ba2e6geg11e4a16e6a15e2g3e8aebae21de2d11e3f8g3e5g3e6g3h5g3e47de5d31e5f4e25a2e197a"
  "13ae2f2eg2e11d2e6de5g3f5g71e144a5e4g11eg2e15a3e10a8e3a2e7g10e2a11e3g15eg3e7g6e11a3e2de18d3ed16e7g7e16gde50d22e2a8e2f7e25a3e196a"
  "14a2f2efge12de7de5g2f5g59ef10e146a6e4g9e2ge16a2e11a8e3a2e4g5e4be4a6e2ge8g6e3g3e3ge7g8e7a2d3e22d2edeg8e4ge8g6edg2d2gdg2dgd2g54d22e4a8e3f4e25a2e2f196a"
  "15a2f2efge7dg4dg6dbdg3de2f2g2hg57e3f8e147a19e2g5ae11a3e12a4e2ae2a4e2g2eae7a13eg2e30g4e9ae2d3e21d4e19g5e65d27e5a3e4a2e2f3e26a2e3f195a"
  "15agf3efge5dedg4dg8dg4df2g2hg56e4f8e147a19e2g4ae13a2e14a2e5ab3e2g2e6aea19e11ge3ge2g4e7g4e9a3d2e22d3e19ge2g2e64d27e7a2e5a2ef3e26ae4f195a"
  "16agf3efg2e2de4dg5dg5deg4dge4g53e7f5ebe106a2e39a11e2g3e4g2e34a2e6a6ege7a9e5g3d3e28g3e10a4e23d5e10g2h9g2e27d2ge8d7g6d2g8d28e15a2ef3e25ae2f2e195a"
  "17ag4ef2ge6d2g4d2g4de2g2ded6g52e4fg2f3ebebe106a2e40a9e5g7e34a3e7a3e3geg5a10e4g4d2e3ge22g5e11a3e24d6e8g3h9ge27d3g8d9g4d2g6d30e16a2ef3e22aeae2f2e195a"
  "18ag4ef2ge5d2g4d2g5d2g5d5g51e8f4ebe108ae41a8e7g5e35a2e7a2be4g2e4a11e3ge3d2e3geg2e2g2e16g5e10a4e24d8e5g4h7g2e27d4g7de13g6d32e3a2e11a2ef2e20ae3a2ef2e195a"
  "19ag4ef2ge2de2dg4d2g5d2g5d2g2h2g49e4fg3f5ebe112ae38a16e38a2e9a2g3e9a11e6g3e2g4e2g2e2de6g2d4g5e10a2d2e2d2e23d4e7g3h10g15dgde5g2de5g2d16g4d36e2b2e12a5e19ae3a2e2fe195a"
  "20a5ef2g11dg6dg6dg2h3g48e8f6eb112a2e37a2e2a12e38a2e10agegeg3ae4a11e6g4e2g3e2ge6d3g3de2ge2g7e6a2d2e2d3e22d4e4g2hg3h12g14d16gd17g40eb2e13a5e19a4efgfe195a"
  "22ag3ef2gdg5dg6dg10d5g30eb17e4fg3f8e155a2e27a10e6a4e14a2e2g5ae4a14e5a3e3d2e12d4ge2g5e3g3a4ed5e3d2e16d8gh4g3h16g2d19g2h20g44e12a5e18a2e2fgf3e194a"
  "23ag3ef2gdg4d2g5d2g8ded4g32eb14e8f10e155ae21a17e6a3e16a2g11a2e2a3e3a3e5a3e4d2e10de4gegeg5e4g6ed5e2d2e16d14g4h36gh20g45e11a5e17a3e4f3e194a"
  "24ag3efe2dg3d2g5d2g11d2g24e3f20e6fg2f10e173a6e11g4e8a2e27ae19a2e5d2e4dede2d2e9g13e28d7gh5g6h41g2h15g38e5b9a5e16a2e6f4e193a"
  "25ag3egedg4dg6dg12d2g22e7f4eb12e4fg3f11e156ae9a13e11g4e30a4e23a2e6d2e5d7e6geg13e24de9g2h7g5h40gh16g38ea3b9a6e9a7e2f3efg2eae193a"
  "26ag5e8d2ge15dge2g18e9f15e10f10e156a18e10g8d3e50a3e4a2e6d2e5d8ege6g20e7de2de4ge26gh4gh17gh31g39e4b9a2e10a2e8f2ea3e2ae193a"
  "27a2ge3ge6d2g12dg4de3g17e3f2g4f4eb10e3f2e5f9e156a6e2g11e8g11d3e9ad40a2e4a2e7d2e5d8e8g12e3d4e2d2e7d14g2f15g2h4gh16g2h29geg38e2a2b19a11ef2e200a"
  "30a4eg3dg15d2g2dg3d20e7f3eb11e3f2e5f8e156a9e16g15d2e57ae8d3e4d8ege6g10e12d5egd14ge6f6g7h4gh46g2f39ea2b19a5e5aefe201a"
  "32a3eg18d2g2d2g3d21e5f20efe2f8e157a9e15g16d2e57ae9d3ede2d8e7g9e11ded5e16g5e2f8g6h3g2h20gh24g2f40eab19ae6a2eaefe201a"
  "34a2e2gdg14d2g3dg4d21e2fe2f3eb26e158a9e13g19d5e53ae11de5d9e3geg9e13de6d14g7e2f9g4h2g2h20g2h23g2f40eb19a4e2ae3fea2e201a"
  "36a2egdg14dg4d2g3d21e5f4eb22e36ae106ae16a9e9g24d4ed2e16a5e29a2e16d10e5g2f6e18d2e4d4g5de4d5e3f13g7h14g2h5gh17g3f40ea2b15a3e2feaef2ef204a"
  "37a3ede19d2g4d20e4f3eb23e144ae15a9e7g35d2e12ae4de29ae17d10e4g2f6e21de2d7g7d7e2f15g5h2gh11gh5g2h17g2f41eab16a3egfeafeaf204a"
  "37a3e13de8de4d20e5f26e159a6e9g38d3e10ae6d2e26ae17d11e5g2f4e26de4g2de6d7e2f16g6h3g2h31g2f38eb2e17aeaefge2ae206a"
  "39ae12d2e6d2e7d19e4f3eb22e159a6e8g40d5e7ae8d2e11a4e9a19d10e5g2f5e25degdgdg3de6d7e2f16g4h3g3h30g3f37e2b2e17aeaefe209a"
  "42a4d2b10d2g11d19e2f27e159a7e4g43d14e20d5e30d8e5g2f4e23ded3geg2de10ded4e4f15g8h2g7h9g2h9ge3f23e2a16e18aefe78ad130a"
  "44a3d2b10dg12d24ebebeb19e158a7e3g48de30d4e32d6e5g2f5e2de22d2gdeg13d9e2f15g15h10gh9g5f22e2a16e18a3e209a"
  "45a3d3b9d2gf9d18e5b2c2e2c13b4e2f140ae17a5e2g83d3e2da2d2g26d4e2a2e6g6e23dg2d2g13d10e3f14g2h2g4h3g5h15gfg7f26e2a11e19ae209a"
  "46a3d3b9d2g10d16e2b4a2b3cbc15be2fe139a2e16a5e2g84d2e3dad2g27d3e5ae4g7e21de2gd3g12d12e5fgf12g3h4g5h16g5fe3f25e2a12e228a"
  "44ad2a3d3b4de16d14e9a3b2c15ab4e144ae2a2e7a92de4da2dgda28d6aeg11e21d3gdg13d13efe5f37g4fgf2e2f38e17ae117ad92a"
  "36ad10a5d2b21d13e12a2b16ab4e142ae3a3e5ae93de4d2a2da29d6aeg11e22de2ge12d20ef37g3fg6f37e17ae210a"
  "46a7d3b6de2g10d12e33a4e151ae96de4d2ada30d6a13e22dedg12ded19e2f36g12f35e227a"
  "49a5d3b7dg9ded11e33a5e149a2e96de5d4a30d7a15e3de15d2g14d20e3f33g7fefe3f35e227a"
  "52a3d4b17d11e34a4e149a2e97de5d4a30d8a15e17d2g14d22e5f30g6fe5f34e228a"
  "54a3d3b15d13e33a2e2fe148a2e98de5d5a30d8a3e3a9e15deg14d26e3fgfe2gf22g3fg2f2e4f34e15ae212a"
  "55a2d4b6dg8d13e33ae2f2e146a3e98de6d4a31d12aea9e30d29e2f3e2f4g2f3gf13g2fg2fe4f34e15a2e211a"
  "55a3d4ba6dg7d12e34a2f2e146a102de7d4a29de3ae7a2e2a7e14ded2e11d32e3f4ege3f4g4f5g2f2g8fe2f3e2f27e228a"
  "56a4d3ba5d3g6d11e34a2f2e3ae141a104d2e6d3a32da2e6a2d6a3e6d2e2ded3e2de9d34e3f6e3f4g4f5g2f3g10f3e2f27e163ad64a"
  "58a2d5ab2de6dg4d9e3b40a2b138a106de6d3a32dae5a4d20a4e10d44e4f3eg6f6g10f32e3a2e7ae37ae178a"
  "59a2d4a2b2e11d9e3b40a2be137a106de6d4a20de12de3a6de21a2ede7d44e5f2e2g5f6g10f2ef28e4a2ef6ae216a"
  "60a2d5a2b11dg8e47a2b134a106de6d6a43de21a2e5d50e3f2g2fe2f2g2f2g16f22e7a2e2f222a"
  "60a2d5a2be10d2g8e46a2be133a106de7d6a20de23de21a4d21e2f29e2f2g2f3efg2f3g10fefe2f21e9a2ef222a"
  "71ae5de5g8e34a5e13a2b128a70d2g33de8d6a4de11dg30de18a3eaed18e2f27e2a2f2g2f3efg4f3g11fe5f11e2a3e10a3e176ae45a"
  "73ae3de5g9e32a9e10a2b128ae69d3g32de8d7a15d2g30de21ae2d17e3f26e2a2f2g3f3e5f3g12fe4f11e2a3e11a2e176a2f44a"
  "74aeded5g11e29a3e4a6e7a2be127ae69d5g3dg26de9d6ae14d2g31de19a2e3d16e2f27e2af3g3f4e4f4g10fe5f11e3a2e12ae176a2f44a"
  "75aede2g3e2g9e38a7e134a71d10g23d2e10d7a13d3g31d20a2e2d2ea6e2f2e2f23e9aef2g4f3e5f4g13f4e3b2e202a2e40a"
  "76ade2g3e2g10e40a8e130a71d8g2d3g20de12d6ae12d3g31d22a5ea5e2f2e3f22e9a2efg4f4e6f2g5f2e3f6e4bae202a2e40a"
  "77ae2f3e3ge3g6e15a5f23a7e128a72d13g19de13d6a45d24a2e3ae2f30e11a6f4e3fe3fg11f4e214aef35a"
  "78a2f4e8g5e13a6f23a8e128ae77d6g20de13d6a44d29a2e2f6ef21e12ae6f4e2f2e14f4e214a2f35a"
  "78a2f3ef5g3eg6e10a8f32a6e121a80d2g22d2e12d7a42d30ae2f4e4f19e15ae4f5e18f3e5a3fe209a3e30a"
  "63ad14aef3e3f2gf3e2g5e10a8f33a9e117ae105d3e11d6a41d30ae2f5e3f18e16a2e4f5e17f3e5a5f208a4e29a"
  "81ae9f3e2g6e7a9f23a3e6a12e3a3ea3b105ae107de12d6a38d32a26e20ae4f5e17f3e2b4a4f208a4e28a"
  "83a3fe7f2ge3f3e6a9f22a4e5a14e3a3e3beae102ae107d2e11d7a35d35a24e22ae4f6e16f3eb4ae3f22afef184a3e28a"
  "88ae15f4e10f54a4b102a108de7d2e2de7a33d36ae2f20e24ae4f6e15f4e29a4ef214a"
  "89a2efe25f54a4b102ae107de12de7a32d37aef20e25ae3f7e18f2e27ae5f213a"
  "95ae20f2e60ae99a3e99de6de3de2dede2d2e7a30d39a3e2f16e26a2e3f3a2e19f2e27a6f212a"
  "96ae20f2e159a4e100de4dede6d6e7a29d39a3e3f15e27a2e2f3a3e18f3e27ae4f212a"
  "98ae19f4e138aeae15a5e96de7d5e2ded7e7ae26d41a3e2f15e27a2e2f4a2e19f3e26ae5f211a"
  "108a4e2f2g13f49a5b95a9e2d3e77d2e7de2ded12ed8e8a11de11d44a4ef13e37a4fefefe12f4e25a3e213a"
  "110a3e2fg13f2e47a5b82ae12a9e2d3e64de2de12de10d22e7ae20d46a4e2f12e38a3f9e9f4e24a3e213a"
  "113a2e3f5g8fe50ae2b93a14e2d4e2d2e50d2e6de6d3e6d3e2d22e2g2e6a3d2g5de3de2d50a5ef10e40a2f10e10f3e24a4e3b208a"
  "115a3e2f3ge8fe48aea2b78aeae13a13e2d4e2d3e53d2e4d3e7d2e2d26e4g2e5aed2eg9e54a4e2f9e40a2f12e8f4e24a3e3b208a"
  "118a2e2f3e8fe49a2b94a17e2d2e3d2e22de2d2e28d3e3d34e8g2e4a9e59a4ef10e26a2b12af11e10f3e27a3ef207a"
  "119a6e8fe49a2be94a16e2d2e3d2e4dede15ded2e12de14d4e3d36e8g3e2a6e63a3ef10e26a2b12af10e11f4e26a3e2f44a2e160a"
  "125a9fe30a2b114a31e2dede6d3ede2d15e19d38e9g2e71a14e29af10a2f3e2a2e4fe8f3e25af3e2bae204a"
  "127a2fe4fe30a2b2e14ae98a30e2de3d2e2d6ed17ed4ede10d40e8g4e70a13e29af10a2f5a2e2fe10f4e24af3e2b2a2f202a"
  "128a2fe3f2e29a2b113ae2a120e6ge2g2e26a2e44aef11e28a2b9a2fe7ae3fe7f4e32a3f201a"
  "129a2fe2f2e25ae3a2b20ae96a119e10ged15a4d53a2f10e28aeb10afe8ae2fe7f4e28a2f2a3f201a"
  "130a2e3f2e21a4eaea2b119a2e3f112e8gegede11a2e5d54af11e27a2b10af2e8aef2e6f4e28a2f2a4f200a"
  "132a5fe17a8eb7e11ae2a2e100a2e5f109ege8g2e2d8a2e8d54a2ea2fe2f2e2ae38a2f9a3e5f5e23afa3b3a2e3f200a"
  "133afe3fe15a3ef2g3e2b12e4a3ea2e101a2e4f109e10g4e3d3a2e10d55a2e2fef3e3a2e36a2f13a4f4e24a2f3b2a4e2f200a"
  "135ae2f2e5a4e5ae3f2g3e2bg19e104a3e6f106e10g3e18d56a7e4aefe36a2f13a5e24afa2b3a2b2e33a2e169a"
  "136a2e2f2e3a5e4ae4fg3e2bg21e104a2e5f105e12g4e7dede6d58a5e4ae2fe36afe12a4e25a2f2b3a2b2e5ae28ae169a"
  "137ae2f5e2f2a6e5f2g3eg17e4f4e100ae2a9f100e6g2e5g12ede6d59a2e6a3fe36a3e11a2e26a2f2b7ab3a5f195a"
  "139af5e2f3a6e4f3g2eg17e7f2e101a3e7f101e5g3e4g11e2de6d67a3f2e36aefe11ae26a2f2b7a2ba6fe194a"
  "141a6e5af4e4f2g20e10f102ae9f43ef5ef6efef41e11g11e9d67ae2f2e37a2f2e47afab2e6f16ae39aea2b2a5b126a"
  "153af5e2f2g20e11f103ae9f20e4f12e15fe9fef35e12g11e8d68a3f2e37af3e46a2f2b2e4fe16a2e38a2e2b2a5b126a"
  "154a2ef3e3f3g17efe2f2g7fe102a2e12f10efe45f37e10g10e9d68aef3e38a2efe36a2f7a2f2bea2e3f58a2b13a3b117a"
  "155ae2f2e3f4g17e3f2g9f103ae12f9e11fe39f33e10g10e9d69a4e39ae2f2e32a4f7a2f2b4ae2f58a2b11aea3b117a"
  "156a7f2g2f18e4fgeg6f2e3f100a2e22f11a38fef33e6g10ede7d107af5a3efe29ae6f2e84a2b122a"
  "156a8fg2f18efe2f2ge11f2e2fe97a2e9f2e9f11a42f2e5fef2e2fef17e4g10e10d107a2f5a2e2f2e27ae7fe84a2b122a"
  "157a7fg2f18e4f3g11f2e3fe98ae8f2e9f11a40fe3fefe12f15e4g9e2de8d107a2f6ae2f3e26ae7fe84a2b122a"
  "158a4fg4e6f13efef3g2f2g17f98ae2f26a5f2e2a50f14e3g9e10d108a3f5ae5fe25ae8f2e206a"
  "158a4fg4e7f13e4fg2f2g9fb8f127a4f2e3a50f15e2g8e9d109a5f4ae5fe24a2e7f2e206a"
  "158a3f4g16f7e5fe21f2ab134a48f10e2a15e7d109a2b2f2ef3a3e3fe20a3e2f2g6f207a"
  "158a3f3g19f4e29f136a50f7e3a16e5d110a2b3fe2f3a2e4fe19a3e2f2g6f207a"
  "158a3f3g19f4e4fe3fe21f2ab134a50f8e2a15e3de114ab6fa2ef2efe18a7f2g4fe182a4b20a"
  "157a3f2g21f2ef2e4f3e23f136a53f5e2a14e3de115a2b5f2ba3e2f18a7f2g5f182a4b20a"
  "157a3f3g24f2ef2e27f135a53f2g19e2de118a2b6f2af2ef13a2e7f2g7fe12ae81a5b22ae63a2d2ab15a"
  "156a4f2g26f4e28fe134a51f2eg18e2de119a3b6f2a3e12a3e7f2g8fe11afe80a5b22a2e62adea2b15a"
  "156a4f3g59f2ab131a50f23e120a3fa7f16a9f4g6fe5a8f2e76a5b2a4b42a3b37a5b15a"
  "156a4f3g60fb132a17fb32f22e121a4fa6f16a9f4g6fe4a10f2e75a5bea4b42a3b37a5b15a"
  "156a4f2g2fe58f2b132a16fb32f22e126a7f15a8f5g6fe4a2f4a2e2f2e75a5bae4b42a3b37a5b15a"
  "130a2e3ae20a4f3g61fe2bf129a16fb31f11eg11e127a3f2g3f2e11a8f3g8fe4a2f12af12a4f58a5b55a7b44a"
  "156a4f3g31fe28f2ebe2f118ae9a16fb31f3e2a18e128a2f2g3f2ef11a7f3g8fe4a3f11a2f11a5f2a2f53a5b55a7b44a"
  "156ae3f3g60f3b3e3f2ef123a15fb29fgf3e4a16e129a6f2e4f11a15fe4a11f7af8a6f33af76a2b9ae45a"
  "157ae2f3ge59fb4e4f2e2f123a14fb29f2gf2e5a15e130a5f2e5f10a15fe5a10fe6a2f8a6f4a6f99a2b9a2e44a"
  "156a2bef4g43f7e4f6be11f2ef121a13fb29f2gf2e6a14e131a6f2g5f2af5a3e12fb5a6f4ae17a6f3a10f14af6af70a3b57a"
  "156a2be2f3g42f2e6b4fbe20f108ae12a12fb29f2gf3e5a14e133a4f2g5f2a2f5a2e11f2b5a7f22a5f2a14f20a2f67a3b57a"
  "158ae2f4g9f2e27f5eb3e2f5b21f3e4f116a11fb28f2gf3e6a5eg7e136a8fe12a6b3fea2b4a3f2af15a3f8a6f6g8f19a2f125a"
  "158ae2f4g9f2e27f2e4b3e29f2e6f115a10fb28f2gf3e6a13e138a6f2e11a7b4e2b4ae2f2a2f14a4f8a8f6g7f19af124a"
  "158a2ef4g37fe46f4e12a2e98a4f3b2fb29fgf4e5a13e140a6f29afe2afa2f28a8f4g7f15a2faf123a"
  "159ae5g36fe47f5e11a2e99a3f3b2ab31f6e2a14e141a6f28aef2afa3f30a7f3g9f13a2faf122a"
  "160a3d3g67fe7fe4fe3f2e2f6e107a2fa36f22e143a3f2e28aef8a2f27ab8f4g7f2a4b5a2f124a"
  "160a3d4g64fe7f4e8f11e105a2fa37f22e34ae108a2f2e29a2e7a2f27a3b7f5g7fabfa7f7af116a"
  "161a3d3ge62f3e6f4e10f9e106afae36f22e34ae110a2e30ae8af27a4ba6f4g8f2ab2a4f9a2f114a"
  "163a2d3ge61f5e5f3e11f9e106a2b34fb8ef15e148ae4a4b56a6b7f7g6f16a2f51ae61a"
  "164a2d3g60f5e2fe3f6e8f9e106a2b34f2b8ef14e147a3ef2a4b2fe3a3e47a6b8f7g4f18a2faf48a2e60a"
  "165a2d3g2fe56f8e5fe2f2e8f9e107ae33f2eb10ef12e147aef3e5f9e43af6ab19fgf21a2fa2f43a2e60a"
  "167ad3g42fe14f8e7fef2e8f9e107aefe31f2e2b9e2f11e149a17e43a3f4ab19f2g2f24a2faf40ae60a"
  "167a5d56f9e6f4e7f11e42ae64ae33f2e3b7e4f10e158a7e3a2e3ae6a2be3a2b3fe5a5bfa4b9a4b9f3b4f3g18afa2f5af100a"
  "168a4d2g39fe14f9e6f4e7f11e107a2e32f3e3b7e4f10e167a3e2a2e5a2b2e2a2b3f2e4a5b2f4b9a5ba6f6b4f3g20af2a2faf99a"
  "36ad132a5dg44f2e6f11e6f5e6f10e108a2e2fe29f5eb10ef10e178ae3a2b2a3e2b4a3b2a2b18a6b9a5fgf6af60a2e55a"
  "170a4d3g44fe6f11e4f6e6f10e109a2e29fef6eb10ef10e177a2e2a2b3a2e2b4a3b2a2b18a6b9a2b6f3af21a2f40ae55a"
  "172a3d3ge33f4e6fe6f11e3f6e5f11e110ae32f12eb15e224abf3b9a6f2af117a"
  "172a4d2g34f4e5fe5fe2f10e3f6e5f11e110a3e30f13e2b14e223a5b13a4f118a"
  "174a4d3g31f5efe3f2e2fe4f10e2f6e5f11e111a3e5fe20fe2fe9f5eb13e199af4ae18a3bfa4b14af16ae99a"
  "174a4d4g29fe2f4efe3f3e5f10e2f6e4f12e111a4e25fe2fe10f4e2b13e126ae71a2f2afef17a3b2f4b131a"
  "175a4d4ge30f6e4f2e6f10ef7e2f12e113a3e27fe11f5ebef11e18ae179afefa3f4a2f11a3b2f4b131a"
  "176a2e2d6g15fef2e10f6e4f3e5f17e2f11e114a3e38f6ebe6f7e6ae10a2e179ae12f3ab6a3b3f6b128a"
  "178a3d6g17f6e6f7e2f4e4f17e3f5ef4e114a4e37f6eb2e7f5e16a4e179ae13f2b6a3b4f5b128a"
  "180a3d7g14f7ef2e4f32e2f5e2f4e114a4efe17fe7fe7f9eb2e8f4e15a4e179ae2f3e8f2b9ab4fa7b89ae3a2e29a"
  "181a3d8g4e7fe2f8e6f30e3f4e3f3e115a4e12f2e4fe14f3e2f6ebe9f3e15a5e178ae2f4e8fb9a2b5f6b89a2e3ae29a"
  "183a2d11g10f10e8f26e3f5e3f2e115a4e11f3e9f2e2f2e3f3ef10e8f5e14a5e171a2fe2a5f5e8f4a7bf2e3fa6b122a"
  "184a2d12g8f12e8f24e4f4e3f2e115a5e10f3e10fefef2e3fe2f11e7f5e14a6e169a5fae3f7e7f4a7b2f2e3f6b122a"
  "60af124a4d7g2a2g7f13e4f28e2f4e5fe74a2e39ad3e11f2e8fe3f3efefe4f13e4f5e13a6e2f167a9f11e6f11bf4e2fa6b44ae75a"
  "186a5d5g2a3g8f43e2f4e5fe74a2e39a2d5e8f4e6fefef4e8f13e3f5e13a6e2f167a9f13e5f10bf5efa6b120a"
  "189a4de4g2e2g9f41e2f4e5fe116a3d4e8f3e10fe2f2e5f23e11aef2eg3e3f164a11f14e5fe7bef5e2fa5b43af76a"
  "190a5d3g2e3g8f41ef4e6fe116a3d6e6f3e6fe3fe3f2e5f21e11ae2f2e2g2e3f163ae8f7ed11e4f7bef6efa5b43a2f75a"
  "192ae2d3g2e3ge8f40ef5e5fe117a2d8e4f4e5f2ef3e2f2e5f20e12ae2f2e2g2e3f162a2e7f8e10d4e3f6b9efa4b44a2f2ae71a"
  "21af172a2e2d8g2f2e3f45e5f2e116a4d8e2f6e2f13e4f18e13ae2f2e2g2e3f162a3e4f9e14d8e4b8efae7b115a"
  "21af174aed9g2f2e2f46e4f2e117a4d32e2f17e15af4eg2e3f162a2eb13e11de4d6e4b9efa7b42aeae70a"
  "198a2d3gh4gf50e4f2e118a4d49e16a8e3f160a2b2e7d5e20d18ef2a5b115a"
  "199a2d3gh4gf50e3f2e119a6d46e17a8e2f160a4e8d3e16d2e3d3ed15efb2a3b45ae69a"
  "200a3d2g2h4g50e2f3e119a7d44e18a4eg4ef156a3b2e36de3d5ed11e2a5b111a"
  "200a3d2g3h3gf49e2f3e120a7de3d39e18a4e2g3ef156a3be41d3e2d13e3a3b43ae67a"
  "201a3d2g3h3g54e121a8d3e3d4e5d26e18a2fg6ef19ae134a6ed2e42d3e4d9e2a7b106a"
  "201a3d3g2h4g53e122a7de4de12ded21e18a2f2g5ef19a2e132a6e2d2e44de6d9e2a5b28ae5ae5ae65a"
  "3ae198ae2d4g2h2g45eg7e123a28d20e17a4fg4ef16a3e133a5e6d2e51d8e2a6b27a2e5ae69a"
  "3a2e198a2d5gh2gf44eg7e123a29d19e18a3f2g3ef17a2e131a8e2de2d2e55d5e2a5b29a2e73a"
  "203a2d2gh5gf42e2g7e125a30d18e18a2f2g3ef149aed3e8de26d2e12d2e10d2e4ded4ea6b29a2e71a"
  "203a2d2g2h4gf42e2g7e125a31d17e18a2f2g3e2f148a5e60dede4d2ed4ea5b102a"
  "203a3d2gh3g2f43eg6e127a31d17e18af2g4ef148a5e7d2e54de3d2ed6e4b102a"
  "204a3d2gh4g48e130a32d15e19a2f5e148a3e6de52de6de7d6e7b97a"
  "204ae2d2g2hg2f46e133a31d15e20af5e148a3e4de53d2e4d2e10d5e6b97a"
  "204a3d2g2h3g43e137a31d14e21a5e147a4e10de31de29d2e4d4eba3b97a"
  "204a3d2g2h3g42e139a30d14e21a5e148a3e10de29de28d2e2d2e4d5e2ab97a"
  "205a2d2g3hgfg41e140a29d13e24a3e149a2e3de12de47dede4de5ded6e2a4b93a"
  "205a3dg3h4g40e140a28d14e25a2e149a2e16d2e46d3e4d2e7d6ea4b93a"
  "205a3d2g2h4g29e2f7e143a26d15e176a2e43d2e13de21d7e2b95a"
  "206a2d3gh4g29e4f5e143a25d16e177a2e8de66de4d7e96a"
  "120ae86a2d3gh3g29e4f6e143a23d16e179a3e63de6dede3deded5e96a"
  "207a2d4gh2g30e3f6e144a23d15e180a4e60d2e6dede7d6e95a"
  "207a2d4gh2g32ef7e143a23d15e181ad4e66de9d7e94a"
  "208ad7g40e144a15de6d5e3g6e183a2ed2ed2e71d2e2f4e94a"
  "208a2e6g40e145a16de4d7e3g4e184a2ede8de59d2e4d3e4f2e93a"
  "209a3e4g40e147a18d11eg3e186a4e58de7d2e4d4e2fgfe31ae61a"
  "209a3e4g39e149a12de2d13eg3e187a4e66de4d3e3fgf2e31ae60a"
  "209a2e3g40e151a13d11e3g4e187a2f3e4de64d3e3f2gf2e92a"
  "209a2e3g40e152a2de8d12e3g4e188a2f3e3d2e4de58d3e3f2gf2e92a"
  "209a2ef3gf38e153ae5de4d10e4g4e189a2fedede14de5d2e7d3e12de21d2e3f2gf2e92a"
  "209a2e2f2gf37e155ae4de5d9e3g4e191af5e13d2e5d13e11de21d2e3fg2f3e91a"
  "210a2e2f3g35e157ae4d21e192af6e4d3ed2e3d19a5e5d2ede17d3e6f2e92a"
  "211a2ef4g34e158a2e2d20e193af6e2ded5e4d22a3e3d4e9de7d4e5f3e92a"
  "194ae16a3ef3g34e159a23e193a2f9e2ded5e25ae3d2a3e6de6d6e4f3e93a"
  "212a4e2g25ea8e159a22e194a3f16e26a4e2a3e14d5e4f3e93a"
  "212a4e2g25e2a6e161a20e195a4f14e28a3e2a4e14d3e5f3e93a"
  "212a2e4g27e169a5e3a5e202a3f4e37ae2a7ede2d2e5d4e7f2e93a"
  "212a2e3gf27e169a4e212a3f2e41a8e3de4d6e7f2e93a"
  "212a2e2fgfef26e437a2e2de5d4e3f2g2f3e52a3e38a"
  "212a2efe2g2f27e436a2e2d3e3d4e3f2g2f3e53a3e37a"
  "212a2e2f2g3f26e438a2e2d8e2f4g2f2e53a3e37a"
  "212a2e2f2ge2f26e440a2d9ef4g2f2e54a3e36a"
  "212a2e2f3g3f25e442a9e6f4e54a4e34a"
  "212a2e3f2g4f20e447a17e56a5e32a"
  "213aeg3fg4f16e455a2e2a5e60a6e31a"
  "213ae2g2f2g3f16e459a5e60a8e29a"
  "213a2eg2f2g3f15e462a2e61a5ef3e28a"
  "214a2e3fg4f15e524a6e2fe28a"
  "214a2e3fg4f15e525a5e2fe28a"
  "214ae2g2fg4f14e529a2f3e28a"
  "215aeg2fg4f7e2ded2e530a2f3e28a"
  "216a2e2f2g3f6e5d2e465a3f2e52a2g5e2a2e29a"
  "216aeg2f2g4f4e6d2e465a4f2e51a3g4e2a2e29a"
  "217a2e2f2g3f3e5ded2e465a2f2g2e50a4g4e33a"
  "217ag2ef2g4f2e8de466af2gfe50a2g5e34a"
  "218a3e3g3f2eada4d2e467a4fe48a3g5e34a"
  "219aef3g4f3e5d2e469a2fe48a3g6e33a"
  "219agf4g2f5e2d4e518a4g5e35a"
  "219agf4g3f4e2d4e518a4g5e35a"
  "220af4g3f4e2d3e519ae3g5e35a"
  "219ae2fg2hg3f4e3d520ae3g5e35a"
  "221afg2hg3f5e2d520a2eg6e35a"
  "219af2bf2g4f7e521a7e36a"
  "219af2bf2g4f8e520a6e37a"
  "220ab3f2g4f7e238a3e280a4e38a"
  "220a4f2g4f7e239a2e280a4e38a"
  "221a3f3g2f9e562a"
  "222a2f3g3f8e562a"
  "222a3f2g3f8e562a"
  "222a2fb2g3f8e562a"
  "223a2f2g4f7e562a"
  "223a2f2g6f4e308a2e253a"
  "224af2g7f3e308a2e253a"
  "224af2b2g7f2e18ae2a2e539a"
  "225a2b4g6fe18ae3a2e538a"
  "227af4g5fe562a"
  "227a2f3g4f2e562a"
  "232a2b2ge3b2g558a"
  "232a2bag4b3g557a"
  "232a2f4b5g3f554a"
  "233af4b5g3f554a"
  "236a2g7f67ae487a"
  "237ag7f67a2e486a"
  "239a5f69ae486a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "251a2g547a"
  "249a4g547a"
  "249a3g548a"
  "249a3g548a"
  "249a3g548a"
  "249a3g548a"
  "249a4g547a"
  "249a4g547a"
  "249a4g547a"
  "249a4g547a"
  "249h4g547h"
  "250h4g546h"
  "249h5g546h"
  "249h5g546h"
  "249h6g545h"
  "249h6g545h"
  "249hgh5g544h"
  "250h7g543h"
  "250h3gh4g542h"
  "251h3gh3g542h"
  "251h7g542h"
  "800h"
  "800h"
  "800h"
  "800h"
))

(area (aux-terrain river
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "8ae6m785a"
  "9a6=<784a"
  "800a"
  "800a"
  "78ai721a"
  "78ady720a"
  "79ad}i718a"
  "80ab?y717a"
  "82ady716a"
  "83ahq715a"
  "83agL715a"
  "83agL715a"
  "74acy7agL715a"
  "75ad}i5acXi714a"
  "18acy54aempO}2mi2ab?y713a"
  "19ahq54a2=<b2=?}i2ady712a"
  "19agXi60ab?}2mpy711a"
  "19agU?y61ab3=Cq710a"
  "20a=ab65acL710a"
  "90ai709a"
  "90ab709a"
  "91acq707a"
  "800a"
  "92acq706a"
  "92ae707a"
  "93a<706a"
  "92acq706a"
  "94ai705a"
  "94af705a"
  "95a<704a"
  "94acq704a"
  "800a"
  "95acq703a"
  "97aiam700a"
  "97ad}j<699a"
  "98ab=ai698a"
  "100acr698a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
))

;; First 60 should be largest features

(area (features (
   (1 "ocean" "Arctic %T")
   (2 "ocean" "Atlantic %T")
   (3 "ocean" "Indian %T")
   (4 "ocean" "Pacific %T")
   (5 "continent" "Europe")
   (6 "continent" "Asia")
   (7 "continent" "North America")
   (8 "continent" "South America")
   (9 "continent" "Africa")
   (10 "continent" "Australia")
   (11 "continent" "Antarctica")
   (12 "sea" "South China %T")
   (13 "sea" "Caribbean %T")
   (14 "sea" "Mediterranean %T")
   (15 "sea" "Bering %T")
   (16 "gulf" "%T of Mexico")
   (17 "sea" "%T of Okhotsk")
   (18 "sea" "%T of Japan")
   (19 "bay" "Hudson %T")
   (20 "sea" "East China %T")
   (21 "sea" "Andaman %T")
   (22 "sea" "Black %T")
   (23 "sea" "Red %T")
   (24 "sea" "North %T")
   (25 "sea" "Baltic %T")
   (26 "sea" "Yellow %T")
   (27 "gulf" "Persian %T")
   (28 "gulf" "%T of California")
   (29 "sea" "Caspian %T")
   (30 "lake" "%T Superior")
   (31 "lake" "%T Victoria")
   (32 "sea" "Aral %T")
   (33 "lake" "%T Huron")
   (34 "lake" "%T Michigan")
   (35 "lake" "%T Tanganyika")
   (36 "lake" "%T Baykal")
   (37 "lake" "Great Bear %T")
   (38 "lake" "%T Nyasa")
   (39 "lake" "Great Slave %T")
   (40 "lake" "%T Erie")
   (41 "lake" "%T Winnipeg")
   (42 "lake" "%T Ontario")
   (43 "lake" "%T Balkhash")
   (44 "lake" "%T Ladoga")
   (45 "lake" "%T Chad")
   (46 "lake" "%T Maracaibo")
   (47 "lake" "%T Onega")
   (48 "lake" "%T Eyre")
   (49 "lake" "%T Volta")
   (50 "lake" "%T Titicaca")
   (51 "lake" "%T Nicaragua")
   (52 "lake" "%T Athabasca")
   (53 "lake" "Reindeer %T")
   (54 "lake" "Issyk Kul %T")
   (55 "lake" "Torrens %T")
   (56 "lake" "Vanern %T")
   (57 "lake" "Nettilling %T")
   (58 "island" "Anticosti %T")
   (59 "island" "Ascension")
   (60 "island" "Azores")
   (61 "island" "Bahamas")
   (62 "island" "Bermuda")
   (63 "island" "Bioko")
   (64 "island" "Canary %Ts")
   (65 "island" "Cape Breton %T")
   (66 "island" "Cape Verde %Ts")
   (67 "island" "Faeroe %Ts")
   (68 "island" "Falkland %Ts")
   (69 "island" "Fernando de Noronha")
   (70 "island" "Great Britain")
   (71 "island" "Greenland")
   (72 "island" "Iceland")
   (73 "island" "Ireland")
   (74 "island" "Madeira")
   (75 "island" "Marajo")
   (76 "island" "Newfoundland")
   (77 "island" "Prince Edward %T")
   (78 "island" "St. Helena")
   (79 "island" "South Georgia")
   (80 "island" "Tierra del Fuego")
   (81 "island" "Tristan da Cunha")
   (82 "island" "Antigua")
   (83 "island" "Barbados")
   (84 "island" "Cuba")
   (85 "island" "Curacao")
   (86 "island" "Dominica")
   (87 "island" "Guadeloupe")
   (88 "island" "Hispaniola")
   (89 "island" "Jamaica")
   (90 "island" "Martinique")
   (91 "island" "Puerto Rico")
   (92 "island" "Tobago")
   (93 "island" "Trinidad")
   (94 "island" "Virgin %Ts")
   (95 "island" "Andaman")
   (96 "island" "Madagascar")
   (97 "island" "Mauritius")
   (98 "island" "Pemba")
   (99 "island" "Reunion")
   (100 "island" "Seychelles")
   (101 "island" "Sri Lanka")
   (102 "island" "Zanzibar")
   (103 "island" "Balearic %Ts")
   (104 "island" "Corfu")
   (105 "island" "Corsica")
   (106 "island" "Crete")
   (107 "island" "Cyprus")
   (108 "island" "Malta")
   (109 "island" "Rhodes")
   (110 "island" "Sardinia")
   (111 "island" "Sicily")
   (112 "island" "Aleutian %Ts")
   (113 "island" "Bougainville")
   (114 "island" "Canton %T")
   (115 "island" "Caroline %Ts")
   (116 "island" "Choiseul")
   (117 "island" "Christmas")
   (118 "island" "Clipperton")
   (119 "island" "Easter %T")
   (120 "island" "Fiji")
   (121 "island" "Funafuti")
   (122 "island" "Galapagos %Ts")
   (123 "island" "Guadalcanal")
   (124 "island" "Guam")
   (125 "island" "Hawaii")
   (126 "island" "Oahu")
   (127 "island" "Hokkaido")
   (128 "island" "Honshu")
   (129 "island" "Iwo Jima")
   (130 "island" "Kyushu")
   (131 "island" "Okinawa")
   (132 "island" "Shokaku")
   (133 "island" "Kodiak")
   (134 "island" "Marquesas %Ts")
   (135 "island" "Marshall %Ts")
   (136 "island" "Nauru")
   (137 "island" "New Caledonia")
   (138 "island" "New Guinea")
   (139 "island" "New Zealand")
   (140 "island" "Leyte")
   (141 "island" "Luzon")
   (142 "island" "Mindanao")
   (143 "island" "Mindoro")
   (144 "island" "Negros")
   (145 "island" "Palawan")
   (146 "island" "Panay")
   (147 "island" "Samar")
   (148 "island" "Sakhalin %T")
   (149 "island" "Samoa")
   (150 "island" "Santa Catalina")
   (151 "island" "Tahiti")
   (152 "island" "Taiwan")
   (153 "island" "Tasmania")
   (154 "island" "Tonga")
   (155 "island" "Vancouver %T")
   (156 "island" "Vanuatu")
   (157 "island" "Bali")
   (158 "island" "Borneo")
   (159 "island" "Celebes")
   (160 "island" "Java")
   (161 "island" "Madura")
   (162 "island" "Moluccas")
   (163 "island" "New Britain")
   (164 "island" "New Ireland")
   (165 "island" "Sumatra")
   (166 "island" "Timor")
   (167 "desert" "Arabian")
   (168 "desert" "Atacama")
   (169 "desert" "Chihuahuan")
   (170 "desert" "Death Valley")
   (171 "desert" "Gibson")
  )
  "800a"
  "19ba7b2a11b3a65b16a7b39a18ba47ba160ba121ba37b7a149ba7b57a18b2a2b"
  "28ba80b15a8b38a388ba163b56a18b2a2b"
  "29ba80b15a8b37a552b56a18b2a2b"
  "29ba81b15a8b37a551b56a18b2a2b"
  "55b2a55b15a8b36a545ba5b56a8b12a2b"
  "54b3a10ba45b15a8b36a165b3a361b2a13b63a7b12a2b"
  "55b2a56b16a8b35a314b2a212b5a11b63a7b13ab"
  "55b2a57b17a7b35a523b12a8b64a7b12ab"
  "13ba100b18a7b34a28ba36b2a162ba290b16a6b66a7b11ab"
  "115b18a7b32a29b2a35b3a30b10a7ba6ba53b2a51b2a287b91a7b11a"
  "115b19a7b31a29b7a30b4a10ba13b31a52b3a7ba42b3a118ba165b93a8ba8ba"
  "7ba16ba90b19a7b31a10b7c12b32a5b5a9b57a39b18a6b5a6ba7b57a14b6a43ba11b2a162b96a17b"
  "25ba89b20a7b30a10b8c5b7c108a27ba9b108a6b12a29b11a10b7a111ba8ba12b5a18b97a17b"
  "115b20a8b29a10b9cb11c108a17b2a4b143a23b31a104ba4b3a7b2a6b19a5b102a16b"
  "16ba97b22a8b28a10b22c111a13b150a20b35a67b27a4b154a15b"
  "73ba38b25a7b28a11b22c112a11b152a17b38a65b186a14b"
  "27ba45b2a36b27a7b28a7ba2b41c94a10b154a14b42a2ba2b10a9b5a18ba14b188a12b"
  "27b2a25b11a8b3a35b28a7b28a6b3ab48c86a10b158a7b46a3b13a2b16a27b190a8bab"
  "27b3a22b33a26b28a7b29a3b65c76a9b212ab35a24b200a"
  "a26b4a7b48a25b29a6b30a2b66c75a9b249a5ba3ba6b207a"
  "2a24b61a7b2a15b30a5b32a67c75a8b257a3bab210a"
  "4a22b72a13b31a4b32a68c75a6b473a"
  "8a12b84a6b68a69c75a5b473a"
  "13a7b158a70c75a4b473a"
  "14a6b158a70c552a"
  "179a70c551a"
  "179a70c551a"
  "179a7ca20c5a26c2a10c550a"
  "179a8c3a14c11a24c2a9c550a"
  "180a26c10a34c550a"
  "180a27c9a34c550a"
  "180a28c9a34c549a"
  "180a29c8a34c549a"
  "180a30c7a35c548a"
  "180a32c5a35c548a"
  "181a7ca64c547a"
  "181a7ca64c547a"
  "181a7c2a63c547a"
  "181a7c2a63c547a"
  "182a7c2a51ca9c548a"
  "182a7c3a48c2a10c548a"
  "182a7c3a37ca9c2a11c548a"
  "183a7ca38c2a21c548a"
  "183a68c338a2*148,209a"
  "184a67c339a2*148,208a"
  "185a65c342a148,207a"
  "185a65c342a2*148,206a"
  "186a64c342a2*148,206a"
  "149a2*76,36a11ca52c342a148,206a"
  "149a2*76,37a9ca54c342a2*148,204a"
  "150a76,39a62c342a3*148,203a"
  "2*155,78a4:66a76,a76,a76,37a17c3a13c4a23c342a3*148,203a"
  "4*155,76a6:50a4V10a76,a76,a76,37a15c10a9ca25c342a3*148,203a"
  "4*155,76a:a5:52a3V8a76,a3*76,2a76,35a14c10a35c343a3*148,202a"
  "79a9:53a2V6a2*76,a6*76,a76,33a13c2a5c3a37c343a2*148,202a"
  "78a11:60a2*76,a76,a7*76,33a12ca47c342a2*148,202a"
  "78a5:2a6:58a2*76,a11*76,32a60c341a2*148,202a"
  "78a5:2a7:57a2*76,3a9*76,33a59c342a148,202a"
  "78a2:14a=a2=62a76,a76,33a28ca31c98a3wa7w233a148,201a"
  "95a2=a2=97a27ca31c98a3w2a7w232a2*148,200a"
  "90a3>4a5=96a25c2a7ca16ca6c95a5w5a5w434a"
  "89a4>5a5=95a34ca16ca7c93a6w7a3w434a"
  "89a4>5a3=a2=95a33c2a16ca10c88a9w2a5w233a3*127,199a"
  "90a3>5a4=97a63c87a10wawa2w234a5*127,197a"
  "90a3>5a=a3=97a45ca17c86a17w232a6*127,196a"
  "91a3>7a2=8a5F85a16ca27ca18c85a22w227a8*127,194a"
  "91a3>7a2=7a6F85a12c5a20ca6ca18c85a23w226a8*127,194a"
  "92a3>7a=99a12c4a20c2a25c41a2*105,41a25w226a7*127,193a"
  "92a4>6a=4a6D89a13c3a21ca25c42a105,42a26w224a7*127,193a"
  "93a3>8a8D91a12c3a22ca12ca12c84a8w8a11w423a"
  "94a2>7a6D95a12c2a22ca25c41a2*110,42a4w16a6w224a2*128,197a"
  "204a12c2a48c41a3*110,291a3*128,196a"
  "204a13ca8ca7ca32c41a2*110,290a5*128,195a"
  "205a12ca8c2a6ca20ca11c29a103,11a3*110,289a5*128,195a"
  "205a21c3a4c2a32c28a103,13a2*110,289a5*128,195a"
  "205a21c3a2X4a32c333a5*128,195a"
  "205a21c3a2X4a32c333a5*128,195a"
  "205a21c4aX5a31c333a5*128,195a"
  "205a22c7aX2a30c333a5*128,195a"
  "206a26c2a2X2a29c331a7*128,195a"
  "206a27c6a13ca14c56a4*111,270a9*128,194a"
  "206a28c5a13c2a12c58a3*111,269a10*128,194a"
  "207a45c3a6ca4c60a2*111,257a5*128,3a13*128,194a"
  "207a44c97a4*106,232a22*128,194a"
  "208a42c119a3*107,217a11*128,200a"
  "208a46c75a108,40a2*107,217a11*128,200a"
  "209a46c337a4*128,204a"
  "209a47c336a4*128,204a"
  "210a47c334a4*128,205a"
  "210a47c543a"
  "211a24ca21c543a"
  "211a24c2a20c543a"
  "211a25ca20c543a"
  "46a2}163a45c544a"
  "47a2}163a18ca25c544a"
  "48a3}161a18c2a24c4a64,112ax426a"
  "49a3}160a7ca10c2a24c4a2*64,112ax425a"
  "50a3}160a17c3a22c11a64,3a64,104ax4ax419a"
  "52a2}159a7ca35c9a64,3a3*64,104a2x2ax419a"
  "53a3}158a6ca35c121a2xax419a"
  "54a3}157a7ca22ca11c2a4c116a4x418a"
  "55a4}156a6c2a21c2a10ca6c116a4x417a"
  "57a3}155a7ca21c2a10ca9c114a5x415a"
  "57a4}155a6ca21c3a20c114a4x415a"
  "59a4}153a6c2a20c4a20c115a4x413a"
  "60a4}152a7ca20c5a20c115a3x413a"
  "60a4}153a6c2a20c4a21c115a3x186a2*152,224a"
  "61a2}155a5c3a12ca6c4a21c115a4x185a3*152,223a"
  "218a6c2a12ca6c5a20c115a6x183a4*152,222a"
  "218a7ca20c2a9ca12c116a6x183a3*152,222a"
  "198a11c10a6ca13ca6c2a10ca11c117a6x182a3*152,222a"
  "189a27c3a6ca13ca7ca10c2a11c116a7x182a2*152,222a"
  "188a30ca20ca19c3a9c117a6x183a152,222a"
  "141a3n44a31ca20ca18c3a9c118a7x404a"
  "125a10n8a3n42a52ca18c4a9c118a6x404a"
  "125a12n9a3n30a61c2a18c2a10c120a6x402a"
  "125a11nan9a9n23a61ca19c2a11c120a6x401a"
  "126a13n3a7n6a6n18a81ca12c121a7x399a"
  "126a14n3a7n10a4n9an5a95c122a6x398a"
  "126a15n3a6n12a3n3a8n4a94c123a7x396a"
  "127a13n5a4n14a3n3a3nana2n4a94c123a7x177a3*141,215a"
  "127a23na25nan4a11ca58c2a21c124a7x175a5*141,214a"
  "128a18na30nan4a8c3a57c3a15ca5c125a7x174a6*141,213a"
  "129a49nan4a7c4a57c3a14c2a4c126a7x174a6*141,212a"
  "129a23na26nan4a7c3a29ca43c134a7x54a4d3a4d109a5*141,212a"
  "130a22na27nan4a7c2a28c2a41c137a7x52a6d2a5d108a6*141,211a"
  "130a2nana3nana5na6na24nana2n3a7c2a27c4a39c139a8x50a7da6d108a3*141,213a"
  "131a10na9na32n3a7ca28c3a33c146a8x49a15d107a3*141,213a"
  "134a16nana28n2a2n3a7ca28c3a33c147a6x48a16d108a4*141,211a"
  "134a7na5na15nX17na4n2a8ca27c3a14c2a17c148a5x48a17d108a3*141,211a"
  "135a3na30na3na8na3n3a7c2a27c2a15c2a17c149a4x47a17d322a"
  "135ananana6na7na24na4n83,n3a7c2a26c3a14c3a16c151a2x46a18d322a"
  "136a29na21n3a8c2a25c3a14c3a16c3a4c145a2x45a19d321a"
  "137an2a8n2a17n2a14na3n4a10ca9c3a11c3a17ca16c2a6c188a22d320a"
  "137a9na2na2n2a11na6na11nanan3a13c3a4c3a10c5a33c2a9c181a28d318a"
  "138a6na6na2n10ana19n7a17c5a9c6a34ca14c169a36d317a"
  "138a13na3n10a8na8na2n7a30c3a38ca16c163a41d316a"
  "151a3n9aJ8a4n17a29c3a60c157a43d315a"
  "163a2J29a28c3a61c155a45d314a"
  "163a2J30a93c153a46d313a"
  "164aJ31a54ca39c150a48d312a"
  "197a54ca21ca17c147a51d311a"
  "197a46ca8ca21c4a14c146a28da23d310a"
  "198a46ca8ca21c4a14c145a28da24d309a"
  "199a77c3a15c144a28da26d2a5d300a"
  "200a77c3a16c142a29da33d86a2*158,211a"
  "202a25c3a48c3a18c139a64d83a4*158,211a"
  "203a75c4a19c137a9da55d3a4d33a165,39a9*158,208a"
  "205a6c2a57ca31c26a6c104a9da36da19d2a5d9a9d14a2*165,38a9*158,208a"
  "214a56ca33c23a9c102a9da36d2a18d2a6d7a12d12a2*165,38a9*158,208a"
  "215a94c14a18c97a9da37d2a17d2a8d4a14d11a3*165,37a11*158,206a"
  "216a103ca22c96a7d3a37da30da16d10a5*165,35a11*158,206a"
  "217a127c94a7d3a37da48d11a5*165,30a13*158,207a"
  "219a28ca101c89a8d2a87d4a4d2a6*165,29a13*158,207a"
  "220a27ca102c89a7d3a87d3a5d4a6*165,26a14*158,206a"
  "221a62c2a66c88a7d3a96d4a5*165,26a14*158,206a"
  "223a58c6a65c88a7d2a97d5a6*165,19a19*158,205a"
  "224a129c87a7d2a98d5a6*165,17a21*158,204a"
  "226a128c86a7da45da53d6a7*165,16a20*158,204a"
  "228a88c2a17ca7ca10c87a53da52d7a6*165,16a20*158,204a"
  "230a74ca11c2a17ca7c5a7c86a54d2a50d7a7*165,15a20*158,204a"
  "130a2*122,3a122,95a86c2a16ca7c6a7c86a54da50d8a10*165,11a20*158,204a"
  "232a85c2a16c2a7c6a7c51a2;32a54da57d2a10*165,11a19*158,204a"
  "233a85ca16c2a7c7a7c50a4;31a63d2a47d2a13*165,10a16*158,204a"
  "234a36ca14c3a47c3a7c7a7c49a5;31a62d2a49da12*165,10a16*158,204a"
  "241a45c2a48c2a7c7a7c49a6;31a62d2a49da13*165,8a15*158,205a"
  "250a37ca48c3a7c7a7c49a5;32a113d2a11*165,9a13*158,77a2*164,127a"
  "251a85c3a8c6a7c49a6;32a39da73d4a9*165,18a4*158,78a2*164,125a"
  "255a33ca15ca31c4a8c5a7c49a6;38a33d2a74d4a8*165,18a4*158,79a164,124a"
  "259a29c2a14c2a30c4a20c50a5;39a73da35d5a6*165,99a2*163,a164,123a"
  "260a28c3a5ca29ca33c52a2;41a13d7a90d4a6*165,99a2*163,a164,122a"
  "261a27c2a70c96a7d12a33da58d4a5*165,98a2*163,124a"
  "262a7c2a17c2a70c115a33d2a18da39d4a4*165,94a6*163,124a"
  "263a7ca17c2a74c111a12da40da41d4a2*165,95a4*163,125a"
  "264a24c2a76c109a53da42d7a160,221a"
  "264a24c2a77c108a53da46d2a4*160,6a3*160,3a3*160,204a"
  "265a24ca78c107a101d2a19*160,203a"
  "266a23ca14c5a60c106a33da20da34da12d3a17*160,203a"
  "266a36c7a60c107a32d2a19d2a34da12d12a7*160,203a"
  "266a37c6a61c106a53d2a48d221a"
  "266a24ca14c4a62c105a54da55d214a"
  "266a24ca16c2a53c3a7c104a54da69d200a"
  "266a24c2a60ca9c3a7c104a54da71d198a"
  "266a24c2a60c2a8c4a7c103a54d2a71d197a"
  "265a25c3a47ca12ca9c3a7c104a54da30da41d32ak4ak21ak136a"
  "265a26c3a47ca11c2a8c3a8c102a55da31da41d31a2k2a3k20a2k135a"
  "265a27c3a7ca51c2a8c3a7c82a96,19a49da37d2a42d29a3ka3k4a2k14a2k135a"
  "265a28c3a6c3a16c4a29c2a8c3a7c82a2*96,18a49da37d3a51d20a11k15a3k134a"
  "265a29c3a5c2a17c4a30c2a8c3a7c80a4*96,17a49da38d2a52d20ak2a8k14a4k133a"
  "265a29c3a6ca5ca10c5a30c2a8c3a7c80a4*96,17a18da30d2a38da53d18a3ka9k14a4k132a"
  "266a19c2a7c3a13ca8c3a33c3a8c3a7c79a5*96,17a7da9d2a29d2a93d18aka11k14a5k130a"
  "266a20c2a7c2a21c4a34c3a7c3a7c79a5*96,17a7d2a7d4a18da9d2a94d12ak2a16k13a6k129a"
  "267a10c3a6c3a6c2a21c4a34c4a7c3a7c78a6*96,16a7d3a28da9d3a94d10a21k13a6k128a"
  "196a2N69a11c2a6c3a7ca22c3a14c5a17c2a7c3a7c77a8*96,15a7d4a27d2a70d2a33d9a24k11a6k128a"
  "196a2N70a11ca7c2a7ca22c3a14c5a18ca8c3a7c76a8*96,15a7d5a98d2a33d8a26k10a7k127a"
  "267a13ca6c3a7ca21c4a14c4a19ca7c3a7c74a11*96,9a12d4a26da73da33d7a28k8aka6k127a"
  "267a13ca7c2a30c3a14c4a20ca7c3a7c72a12*96,3a122da33d6a30k7ak2a6k126a"
  "267a13ca7c2a30c3a46c3a7c72a96,a10*96,2a123d2a32d5a21ka10k6a2ka7k125a"
  "267a14ca7c2a30c2a39ca7c2a8c61a6d4a12*96,2a124da31d4a22k2a14k4a8k125a"
  "267a14ca7c2a30c2a47c2a8c60a8d4a11*96,2a86da26da10d2a30d4a24ka13k4a10k123a"
  "267a23c2a29c2a40ca7ca9c59a9d3a11*96,2a86da9da16da10da31d5a52k122a"
  "267a23c2a30c2a39ca7ca10c58a10d3a2*96,a7*96,2a44da41da27da9da30d5a54k121a"
  "267a24c2a29c3a39ca7ca10c57a11d3a10*96,a44da110d4ka57k118a"
  "267a7ca16c2a29c2a8c4a28ca7ca11c56a11d3a10*96,a14d8a22da6da98d3a65k117a"
  "267a25ca29c2a9c4a27c2a6c2a11c55a12d2a10*96,2a13d8a22da14d3a87d3a67k116a"
  "266a26c2a29ca13ca26c3a5c3a11c55a11d2a10*96,3a12d5a41d2a86d4a68k115a"
  "266a27ca29ca40c3a8ca10c55a12da10*96,4a12d3a42d2a12da72d5a71k112a"
  "266a27ca7ca84c56a11d2a9*96,2a59d2a12da72d5a72k111a"
  "265a29ca60ca22ca8c55a12d2a8*96,2a60d2a58da24d2ak3a74k109a"
  "265a29ca84ca8c55a11d2a9*96,a6da53d3a22da27da6da24d2a2k2a75k108a"
  "265a115c2a7c51a15d2a8*96,a60d2a23da35da23d2a2k2a77k106a"
  "265a30ca44ca40c2a7c50a16d2a7*96,2a60da39da19da24da2k2a78k105a"
  "265a18ca56c2a40c2a7c49a16d3a6*96,a61da60da23da2k2a79k104a"
  "264a18c2a12ca44c2a40c2a7c48a17d3a5*96,a8da78da35da22da2k2a80k103a"
  "262a80c2a40ca8c47a19da5*96,a8da53da60da22d2ak2a81k102a"
  "260a37ca45ca41ca7c47a21da3*96,9da20da32d2a82d2ak2a82k101a"
  "259a84ca16ca25ca7c46a23d2*96,30d2a31d3a82d2ak2a82k100a"
  "258a85ca16ca33c46a55d2a31d4a22da35da22d2ak3a82k99a"
  "258a85ca16c2a13c2a18c44a56d3a30da62da21d3ak2a83k98a"
  "258a102c2a13c5a15c44a57d2a93da22d2a2k2a83k97a"
  "258a13ca72ca14c4a13c6a14c42a58d2a94da22d2a2k2a83k96a"
  "259a10c2a73c2a12c6a12c3a17c42a9da48d2a94da24da2k2a53ka28k96a"
  "259a8c4a73c3a8c10a32c40a10d2a47d3a119d2a2ka56k3a23k95a"
  "260a7c4a73c5a48c40a10d2a48d2a120d2a2ka58k4a20k94a"
  "260a7c4a74c2a53c36a11d2a49da32da88d2a3ka80k94a"
  "261a7c3a74c2a56c33a11d2a49da35da86d3ak2a80k93a"
  "260a8c3a41ca32c2a58c31a10d2a43da130d3ak3a10ka67k93a"
  "258a11c2a15ca58c2a59c30a9da45da131d2ak4a9ka68k92a"
  "258a11c2a15ca58c2a60c29a46da8d2a131d2ak3a11ka66k92a"
  "253a4ca11c3a13c3a57c2a36ca24c29a44d2a8d2a131d3ak2a11k2a10ka54k92a"
  "253a32c3a57c2a13c4a9c3a33c28a90da97d3aka13k3a5k16a41k92a"
  "253a32c3a73ca11c3a17ca15c29a187d3ak2a12ka3k22a39k92a"
  "253a121c2a17ca16c26a190d3aka2ka6k3a3k26a36k92a"
  "254a120c2a18ca16c25a77da112d3ak2a7k4a2k28a14k3a18k92a"
  "254a121ca18c2a15c25a101da89d2a2k3a2k2a7k29a15k2a15k94a"
  "254a121c2a17c2a16c24a91da9da28da21da39d2a5k40a15k2a14k94a"
  "254a91ca30ca18c2a16c19a96da8da29d2a5da54d3a2k44a14ka13k94a"
  "250a49ca42c4a30ca18c3a16c18a61d3a41da29da7da55d51a24k94a"
  "250a29c4a60c3a30ca19c3a18c15a62d2a80da58d49a22k94a"
  "250a30c3a4c2a18ca22ca13c3a49c3a19c14a144da13da47d47a21k94a"
  "250a31c2a5c2a40ca13c3a50c2a20c13a122da35da48d48a19k94a"
  "250a32ca6ca55c2a51ca21c12a122d2a12da8da13da48d49a17k94a"
  "251a39ca40ca13c2a21ca29ca22c11a122d8a6d2a7d2a12d2a48d49a16k94a"
  "251a39c2a39ca14ca20c2a29ca22c11a122d9a5d3a7d2a5da5d3a47d51a14k94a"
  "244a47ca40ca13c2a19c2a29c2a22c10a7da71d8a4d9a21d11a4d3a7d9a4d13a19d3a15d54a8k97a"
  "243a18c2a28c2a39ca14c2a19c2a29c2a10ca10c10a8da66d33a11d21a6d27a10d15a11d55a5k99a"
  "243a18c2a29ca39c2a13c3a18c3a29c2a9c2a10c10a7da67d65a5d53a5d164a"
  "242a17c5a28ca40ca13c2a20c2a29c2a10c2a9c10a7da67d66a4d222a"
  "241a51c2a40ca12c2a12ca7c2a23ca5c2a10c3a9c10a67da7d291a"
  "241a52ca40c2a12ca12c2a7ca23c2a17c3a8c10a7da67d291a"
  "241a52c2a40ca25c2a7ca24ca18c2a9c10a74d291a"
  "241a53ca32ca21ca12c2a7ca42c2a9c10a14da60d290a"
  "241a53c2a40ca12c2a11c2a7ca54c10a14da13da45d290a"
  "238a56c2a40ca13ca12c2a61c11a13da13d2a45d289a"
  "238a56c2a41ca12ca13ca62c10a14da59d289a"
  "238a36ca19c3a41ca11ca13c2a61c10a14d2a59d288a"
  "238a36c2a8ca9c3a41ca26ca61c11a14d2a58d288a"
  "238a36c2a8c2a8c3a41ca26c2a61c10a14d2a58d288a"
  "238a36c2a9c2a7c4a22ca18ca25c2a61c11a14da35da23d287a"
  "238a36c2a9c2a8c3a22ca46ca43ca17c10a14da59d287a"
  "239a34c3a10c2a8c3a41ca88c11a74d286a"
  "239a19ca15c2a10c3a7c3a41ca88c11a25da48d286a"
  "239a20ca15ca11c2a8c2a14c2a6ca10ca8ca88c11a63da9d286a"
  "240a47c2a8c3a13ca8ca9c2a7ca71ca16c11a64da9d285a"
  "240a48c2a7c4a21c2a9ca8ca71ca16c11a64da8d285a"
  "240a48c3a7c3a21c4a7ca8ca88c11a74d284a"
  "240a48c3a7ca23c3a8ca8c2a88c11a73d284a"
  "240a14ca34c2a7ca15ca8c2a8ca9ca88c12a73d283a"
  "240a14c2a33c2a32c2a8c2a8c2a88c12a72d283a"
  "241a13c3a33c2a31c2a8c3a7c2a88c14a71d282a"
  "241a14c6a22ca6c3a51c2a87c15a70d282a"
  "242a13c7a21ca7ca52c2a80ca7c15a70d281a"
  "242a13ca68,2a2*68,3a11c2a7ca6ca142c16a11d2a26d2a28d281a"
  "243a12ca68,3a2*68,4a8c2a8ca75ca74c17a7d4a56d281a"
  "244a11c12a7c2a8c2a75ca53ca20c16a7da59d281a"
  "245a10c4a15c2a16ca16ca26c68,34ca43c2a20c16a66d281a"
  "240a2*80,4a9ca18c2a34ca51ca9ca43c3a19c15a66d281a"
  "240a3*80,4a27c2a34c2a50c2a9c3a7c4a7c2a14ca6c2a19c16a65d281a"
  "238a8*80,2a26c2a35ca42ca7c3a8c3a8c3a8ca22ca20c16a64d281a"
  "238a8*80,3a62c2a42ca7c2a9c3a7c3a8ca43c17a63d281a"
  "236a9*80,5a62ca51ca9c3a8c2a53c17a12d8a41d282a"
  "237a8*80,3a64ca52ca9c2a8c3a53c17a8d16a36d282a"
  "239a5*80,4a64c2a52ca8c3a8c3a53c44a32d282a"
  "248a15c3a39ca7ca44ca16c4a7c4a53c46a28d283a"
  "247a15c2a41ca8ca19ca11ca12ca16c2a9c3a54c47a25d284a"
  "247a59ca39ca13ca16ca9c3a54c51a21d284a"
  "247a32ca26ca28ca10c2a13ca15ca9c3a55c56a13d286a"
  "247a41ca46ca11c2a12ca15ca10c2a55c355a"
  "247a41ca58c2a13ca15ca9c3a9ca44c355a"
  "248a40c2a57c2a13ca15c2a9c3a42ca11c354a"
  "249a41ca57ca14ca15ca10c2a43ca10c354a"
  "250a113ca26c2a10ca43c354a"
  "255a101ca33c2a55c353a"
  "254a102ca34ca11ca43c353a"
  "254a102c2a89c353a"
  "254a102c2a90c352a"
  "254a16c4a41c3a38c2a90c352a"
  "254a48c3a51ca92c351a"
  "255a48ca145c351a"
  "256a121ca9ca61c351a"
  "257a120ca9c2a10ca50c350a"
  "258a119ca9c2a10c2a49c350a"
  "259a118c2a8c3a8c3a42ca7c349a"
  "262a17ca96c4a7c4a7c3a9ca7c2a14ca8c2a6c349a"
  "263a14c4a31c2a7c4a10c8a22c2a8c6a6c5a5c5a8c2a6c3a13c2a7c3a5c349a"
  "265a11c16a18c16a9c33a5c9a4c17a7c3a5c4a11c14a4c349a"
  "296a8c23a7c70a6c13a9c368a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
  "800a"
))

(base 404 191 0 (n "Asmera"))
(base 300 184 0 (n "Bamako"))
(base 673 123 0 (n "Cairns"))
(base 277 240 0 (n "Cordoba"))
(base 637 129 0 (n "Darwin"))
(base 277 270 0 (n "Dover"))
(base 708 62 0 (n "Hobart"))
(base 265 220 0 (n "Las Palmas"))
(base 427 148 0 (n "Mombasa"))
(base 328 180 0 (n "Ogbomosho"))
(base 626 273 0 (n "P. Kamchatkij"))
(base 122 187 0 (n "San Salvador"))
(town 316 169 0 (n "Abidjan"))
(town 324 171 0 (n "Accra"))
(town 368 238 0 (n "Adana"))
(town 680 82 0 (n "Adelaide"))
(town 414 187 0 (n "Aden"))
(town 408 175 0 (n "Adis Adeba"))
(town 467 208 0 (n "Ahmadabad"))
(town 55 234 0 (n "Albuquerque"))
(town 375 236 0 (n "Aleppo"))
(town 484 212 0 (n "Allahabad"))
(town 453 251 0 (n "Alma-Ata"))
(town 362 244 0 (n "Ankara"))
(town 457 116 0 (n "Antananarivo"))
(town 204 106 0 (n "Antofagasta"))
(town 601 248 0 (n "Aomori"))
(town 389 258 0 (n "Astrachan'"))
(town 233 101 0 (n "Asuncion"))
(town 378 210 0 (n "Aswan"))
(town 375 214 0 (n "Asyut"))
(town 343 241 0 (n "Athens"))
(town 106 229 0 (n "Atlanta"))
(town 760 77 0 (n "Auckland"))
(town 397 227 0 (n "Baghdad"))
(town 398 246 0 (n "Baku"))
(town 488 186 0 (n "Bangalore"))
(town 540 186 0 (n "Bangkok"))
(town 591 151 0 (n "Banjarmasin"))
(town 542 245 0 (n "Baoding"))
(town 154 179 0 (n "Baranquilla"))
(town 292 248 0 (n "Barcelona"))
(town 455 273 0 (n "Barnaul"))
(town 226 156 0 (n "Belem"))
(town 257 278 0 (n "Belfast"))
(town 328 253 0 (n "Belgrade"))
(town 257 114 0 (n "Belo Horizonte"))
(town 342 225 0 (n "Benghazi"))
(town 477 208 0 (n "Bhopal"))
(town 277 251 0 (n "Bilbao"))
(town 102 229 0 (n "Birmingham"))
(town 560 267 0 (n "Blagoveshchenk"))
(town 426 123 0 (n "Blantyre"))
(town 222 90 0 (n "Bloemfontein"))
(town 162 166 0 (n "Bogota"))
(town 281 255 0 (n "Bordeaux"))
(town 348 273 0 (n "Br'ansk"))
(town 705 97 0 (n "Brisbane"))
(town 267 271 0 (n "Bristol"))
(town 284 267 0 (n "Brussels"))
(town 342 253 0 (n "Bucharest"))
(town 322 260 0 (n "Budapest"))
(town 112 251 0 (n "Buffalo"))
(town 20 269 0 (n "Calgary"))
(town 159 164 0 (n "Cali"))
(town 703 80 0 (n "Canberra"))
(town 414 83 0 (n "Cape Town"))
(town 278 231 0 (n "Casablanca"))
(town 595 179 0 (n "Cebu"))
(town 580 262 0 (n "Chabarovsk"))
(town 562 251 0 (n "Changchun"))
(town 574 247 0 (n "Changjin"))
(town 551 219 0 (n "Changsha"))
(town 408 275 0 (n "Chelabinsk"))
(town 531 223 0 (n "Chengdu"))
(town 64 219 0 (n "Chihuahua"))
(town 526 271 0 (n "Chita"))
(town 764 62 0 (n "Christchurch"))
(town 104 241 0 (n "Cincinnati"))
(town 106 247 0 (n "Cleveland"))
(town 490 181 0 (n "Coimbatore"))
(town 504 173 0 (n "Colombo"))
(town 291 177 0 (n "Conakry"))
(town 215 99 0 (n "Cordoba"))
(town 162 173 0 (n "Cucuta"))
(town 253 101 0 (n "Curitiba"))
(town 559 191 0 (n "Da-nang"))
(town 506 208 0 (n "Dacca"))
(town 561 244 0 (n "Dairen"))
(town 278 190 0 (n "Dakar"))
(town 83 227 0 (n "Dallas"))
(town 430 142 0 (n "Dar es Salaam"))
(town 357 262 0 (n "Dnepropetrovsk"))
(town 291 269 0 (n "Dortmund"))
(town 352 166 0 (n "Douala"))
(town 259 274 0 (n "Dublin"))
(town 433 90 0 (n "Durban"))
(town 442 240 0 (n "Dusanbe"))
(town 20 273 0 (n "Edmonton"))
(town 60 225 0 (n "El Paso"))
(town 380 243 0 (n "Erzurum"))
(town 412 228 0 (n "Esfahan"))
(town 293 269 0 (n "Essen"))
(town 282 230 0 (n "Fez"))
(town 252 149 0 (n "Fortaleza"))
(town 297 267 0 (n "Frankfurt"))
(town 451 249 0 (n "Frunze"))
(town 82 227 0 (n "Ft Worth"))
(town 585 230 0 (n "Fukuoka"))
(town 568 214 0 (n "Fuzhou"))
(town 314 277 0 (n "Gdansk"))
(town 305 254 0 (n "Genoa"))
(town 240 120 0 (n "Goiania"))
(town 80 201 0 (n "Guadalajara"))
(town 555 208 0 (n "Guangzhou"))
(town 158 152 0 (n "Guayaquil"))
(town 546 203 0 (n "Ha-noi"))
(town 556 198 0 (n "Haikou"))
(town 590 233 0 (n "Hiroshima"))
(town 757 203 0 (n "Honolulu"))
(town 555 227 0 (n "Huainan"))
(town 273 275 0 (n "Hull"))
(town 486 195 0 (n "Hyderabad"))
(town 98 243 0 (n "Indianapolis"))
(town 173 149 0 (n "Iquitos"))
(town 506 271 0 (n "Irkutsk"))
(town 352 242 0 (n "Izmir"))
(town 117 223 0 (n "Jacksonville"))
(town 471 214 0 (n "Jaipur"))
(town 355 282 0 (n "Jaroslavi"))
(town 386 244 0 (n "Jerevan"))
(town 448 232 0 (n "Kabul"))
(town 589 224 0 (n "Kagoshima"))
(town 406 157 0 (n "Kampala"))
(town 597 238 0 (n "Kanazawa"))
(town 337 184 0 (n "Kano"))
(town 480 214 0 (n "Kanpur"))
(town 77 243 0 (n "Kansas City"))
(town 576 207 0 (n "Kaohsiun"))
(town 440 264 0 (n "Karaganda"))
(town 462 275 0 (n "Kemerovo"))
(town 357 267 0 (n "Kharkov"))
(town 388 190 0 (n "Khartoum"))
(town 142 198 0 (n "Kingston"))
(town 374 147 0 (n "Kinshasa"))
(town 586 231 0 (n "Kitakyushu"))
(town 580 267 0 (n "Komsomol'sk"))
(town 318 277 0 (n "Konigsberg"))
(town 473 282 0 (n "Krasnojarsk"))
(town 554 165 0 (n "Kuala Lumpur"))
(town 384 273 0 (n "Kujbyshev"))
(town 317 173 0 (n "Kumasi"))
(town 531 212 0 (n "Kunming"))
(town 417 275 0 (n "Kurgan"))
(town 574 235 0 (n "Kwangjin"))
(town 125 206 0 (n "La Habana"))
(town 198 121 0 (n "La Paz"))
(town 334 172 0 (n "Lagos"))
(town 462 225 0 (n "Lahore"))
(town 278 265 0 (n "Le Havre"))
(town 329 287 0 (n "Leningrad"))
(town 176 131 0 (n "Lima"))
(town 372 139 0 (n "Luanda"))
(town 406 131 0 (n "Lubumbashi"))
(town 291 256 0 (n "Lyon"))
(town 497 186 0 (n "Madras"))
(town 404 273 0 (n "Magnitogorsk"))
(town 453 123 0 (n "Majanga"))
(town 130 184 0 (n "Managua"))
(town 205 151 0 (n "Manaus"))
(town 522 205 0 (n "Mandalay"))
(town 244 77 0 (n "Mar del Plata"))
(town 162 181 0 (n "Maracaibo"))
(town 280 225 0 (n "Marrakech"))
(town 294 252 0 (n "Marseilles"))
(town 424 236 0 (n "Mashhad"))
(town 370 145 0 (n "Matadi"))
(town 373 158 0 (n "Mboudaka"))
(town 396 204 0 (n "Mecca"))
(town 548 166 0 (n "Medan"))
(town 157 171 0 (n "Medellin"))
(town 218 86 0 (n "Mendoza"))
(town 126 215 0 (n "Miami"))
(town 91 251 0 (n "Milwaukee"))
(town 78 253 0 (n "Minneapolis-St. Paul"))
(town 333 273 0 (n "Minsk"))
(town 431 162 0 (n "Mogadishu"))
(town 80 212 0 (n "Monterrey"))
(town 247 84 0 (n "Montevideo"))
(town 457 223 0 (n "Multan"))
(town 305 264 0 (n "Munich"))
(town 484 203 0 (n "Nagpur"))
(town 587 215 0 (n "Naha"))
(town 420 153 0 (n "Nairobi"))
(town 557 219 0 (n "Nanchang"))
(town 276 261 0 (n "Nantes"))
(town 100 236 0 (n "Nashville"))
(town 98 223 0 (n "New Orleans"))
(town 269 278 0 (n "Newcastle"))
(town 297 253 0 (n "Nice"))
(town 600 241 0 (n "Niigata"))
(town 568 221 0 (n "Ningbao"))
(town 400 284 0 (n "Niznij Tagil"))
(town 122 238 0 (n "Norfolk"))
(town 466 273 0 (n "Novokuzneck"))
(town 347 259 0 (n "Odessa"))
(town 592 233 0 (n "Okayama"))
(town 75 234 0 (n "Oklahoma City"))
(town 73 247 0 (n "Omaha"))
(town 435 275 0 (n "Omsk"))
(town 293 235 0 (n "Oran"))
(town 397 269 0 (n "Orenburg"))
(town 562 153 0 (n "Palembang"))
(town 324 238 0 (n "Palermo"))
(town 145 179 0 (n "Panama Canal"))
(town 146 178 0 (n "Panama City"))
(town 493 212 0 (n "Patna"))
(town 627 86 0 (n "Perth"))
(town 426 275 0 (n "Petropavlovsk"))
(town 553 182 0 (n "Phnom-Penh"))
(town 45 230 0 (n "Phoenix"))
(town 111 245 0 (n "Pittsburgh"))
(town 480 195 0 (n "Poona"))
(town 426 83 0 (n "Port Elizabeth"))
(town 345 167 0 (n "Port Harcourt"))
(town 672 137 0 (n "Port Moresby"))
(town 150 199 0 (n "Port-au-Prince"))
(town 9 256 0 (n "Portland"))
(town 270 246 0 (n "Porto"))
(town 253 92 0 (n "Porto Alegre"))
(town 308 267 0 (n "Prague"))
(town 579 235 0 (n "Pusan"))
(town 570 243 0 (n "Pyongyang"))
(town 442 225 0 (n "Qandahar"))
(town 553 260 0 (n "Qiqihaer"))
(town 159 156 0 (n "Quito"))
(town 528 195 0 (n "Rangoon"))
(town 264 140 0 (n "Recife"))
(town 324 280 0 (n "Riga"))
(town 116 249 0 (n "Rochester"))
(town 317 248 0 (n "Rome"))
(town 20 241 0 (n "Sacramento"))
(town 560 179 0 (n "Sai-gon"))
(town 422 116 0 (n "Salisbury"))
(town 40 247 0 (n "Salt Lake City"))
(town 262 129 0 (n "Salvador"))
(town 80 221 0 (n "San Antonio"))
(town 37 227 0 (n "San Diego"))
(town 166 198 0 (n "San Juan"))
(town 82 205 0 (n "San Luis Potosi"))
(town 214 84 0 (n "Santiago"))
(town 142 202 0 (n "Santiago de Cuba"))
(town 157 198 0 (n "Santo Domingo"))
(town 599 252 0 (n "Sapporo"))
(town 604 242 0 (n "Sendai"))
(town 339 249 0 (n "Sofia"))
(town 162 262 0 (n "St. John's"))
(town 88 240 0 (n "St. Louis"))
(town 305 289 0 (n "Stockholm"))
(town 300 262 0 (n "Stuttgart"))
(town 593 142 0 (n "Surabaya"))
(town 471 204 0 (n "Surat"))
(town 404 280 0 (n "Sverdlovsk"))
(town 575 211 0 (n "T'aipei"))
(town 415 280 0 (n "T'umen'"))
(town 393 240 0 (n "Tabriz"))
(town 121 217 0 (n "Tampa"))
(town 384 247 0 (n "Tbilisi"))
(town 124 189 0 (n "Tegucigalpa"))
(town 409 236 0 (n "Tehran"))
(town 339 246 0 (n "Thessaloniki"))
(town 554 241 0 (n "Tianjin"))
(town 457 280 0 (n "Tomsk"))
(town 301 256 0 (n "Torino"))
(town 285 253 0 (n "Toulouse"))
(town 335 223 0 (n "Tripoli"))
(town 50 226 0 (n "Tucson"))
(town 80 236 0 (n "Tulsa"))
(town 316 238 0 (n "Tunis"))
(town 605 146 0 (n "Ujung Pendang"))
(town 515 260 0 (n "Ulaan Bataar"))
(town 515 269 0 (n "Ulan Ude"))
(town 460 264 0 (n "Ust' Kamenogorsk"))
(town 290 244 0 (n "Valencia"))
(town 212 85 0 (n "Valparaiso"))
(town 5 265 0 (n "Vancouver"))
(town 95 199 0 (n "Veracruz"))
(town 315 262 0 (n "Vienna"))
(town 328 275 0 (n "Vilnius"))
(town 576 251 0 (n "Vladivostok"))
(town 377 262 0 (n "Volgograd"))
(town 596 231 0 (n "Wakayama"))
(town 322 271 0 (n "Warsaw"))
(town 73 238 0 (n "Wichita"))
(town 60 264 0 (n "Winnipeg"))
(town 517 236 0 (n "Xining"))
(town 555 232 0 (n "Xuzhou"))
(town 356 166 0 (n "Yaounde"))
(town 317 256 0 (n "Zagreb"))
(town 284 247 0 (n "Zaragoza"))
(town 544 234 0 (n "Zhengzhou"))
(city 365 226 0 (n "Alexandria"))
(city 300 236 0 (n "Algiers"))
(city 550 243 0 (n "Beijing"))
(city 304 271 0 (n "Berlin"))
(city 475 197 0 (n "Bombay"))
(city 128 250 0 (n "Boston"))
(city 242 85 0 (n "Buenos Aires"))
(city 370 221 0 (n "Cairo"))
(city 504 205 0 (n "Calcutta"))
(city 173 179 0 (n "Caracas"))
(city 93 248 0 (n "Chicago"))
(city 535 221 0 (n "Chonqing"))
(city 297 280 0 (n "Copenhagen"))
(city 473 219 0 (n "Delhi"))
(city 56 243 0 (n "Denver"))
(city 102 249 0 (n "Detroit"))
(city 362 262 0 (n "Doneck"))
(city 262 280 0 (n "Glasgow"))
(city 366 280 0 (n "Gorky"))
(city 562 256 0 (n "Haerbin"))
(city 295 274 0 (n "Hamburg"))
(city 563 206 0 (n "Hong Kong"))
(city 86 221 0 (n "Houston"))
(city 350 248 0 (n "Istanbul"))
(city 578 143 0 (n "Jakarta"))
(city 424 99 0 (n "Johannesburg"))
(city 454 212 0 (n "Karachi"))
(city 344 267 0 (n "Kiev"))
(city 594 233 0 (n "Kobe"))
(city 595 234 0 (n "Kyoto"))
(city 270 243 0 (n "Lisbon"))
(city 275 271 0 (n "London"))
(city 31 232 0 (n "Los Angeles"))
(city 280 245 0 (n "Madrid"))
(city 267 274 0 (n "Manchester"))
(city 584 188 0 (n "Manila"))
(city 697 76 0 (n "Melbourne"))
(city 90 201 0 (n "Mexico City"))
(city 304 256 0 (n "Milan"))
(city 115 259 0 (n "Montreal"))
(city 353 277 0 (n "Moscow"))
(city 599 235 0 (n "Nagoya"))
(city 560 227 0 (n "Nanjing"))
(city 122 246 0 (n "New York"))
(city 453 277 0 (n "Novosibirsk"))
(city 595 233 0 (n "Osaka"))
(city 284 262 0 (n "Paris"))
(city 121 244 0 (n "Philadelphia"))
(city 260 107 0 (n "Rio de Janeiro"))
(city 284 271 0 (n "Rotterdam"))
(city 20 239 0 (n "San Francisco"))
(city 254 106 0 (n "Sao Paulo"))
(city 9 261 0 (n "Seattle"))
(city 574 238 0 (n "Seoul"))
(city 560 248 0 (n "Shengyang"))
(city 561 161 0 (n "Singapore"))
(city 706 83 0 (n "Sydney"))
(city 540 238 0 (n "Taiyuan"))
(city 440 247 0 (n "Tashkent"))
(city 378 228 0 (n "Tel Aviv"))
(city 604 236 0 (n "Tokyo"))
(city 110 253 0 (n "Toronto"))
(city 118 241 0 (n "Washington DC"))
(city 553 223 0 (n "Wuhan"))
(city 535 232 0 (n "Xi'an"))
