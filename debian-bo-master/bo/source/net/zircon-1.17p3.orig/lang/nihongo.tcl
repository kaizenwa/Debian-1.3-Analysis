#
# $Source:$
# $Date:$
# $Revision:$
#
# ----------------------------------------------------------------------
#   AUTHOR:  Lindsay Marshall <lindsay.marshall@newcastle.ac.uk>
# ----------------------------------------------------------------------
# Copyright 1996 The University of Newcastle upon Tyne (see COPYRIGHT)
# ======================================================================
#
array set ztrans {
    abort	切断
    accept	受取
    action	動作
    actions	動作
    append	追加
    away	留守
    back	戻
    ban		Ban
    brb		BRB
    busy	忙しい
    buttons	ボタン  
    call	呼出
    cancel	解除
    channel	チャンネル
    channels	チャンネル
    chanop	チャンネルOp
    chat	話
    clear	クリアー
    close	閉
    connect	コンネクト
    crypt	暗号
    ctcp	CTCP
    dcc		DCC
    default	デフォールト
    delete	取消
    dismiss	閉
    draw	引
    empty	空
    error	エラー
    exec	実行
    favourites	人気もの
    finger	フィンガー
    flush	フラッシュ
    get		取
    help	ヘルプ
    history	ヒストリー
    hostname	ホースト名
    info	インフォメーション
    invite	招待
    irc		IRC
    ircname	Irc名
    join	参加
    jump	ジャンプ
    keep	キープ
    key		キー
    kick	蹴
    kill	殺
    leave	別
    log		ログ
    limit	リミット
    list	リスト
    message	メッセージ
    messages	メッセージ
    mode	モード
    moderated	モデレーテッド
    monitor	モニター
    name	名前
    names	名前
    new		新
    nickname	愛称
    nocase	ノーケス
    notice	注意
    notify	通知
    offer	申出
    ok		OK
    open	オープン
    operator	オペレーター
    parameter	パラメーター
    parameters	パラメーター
    password	パスワード
    pattern	パターン
    people	人々
    plugin	プラグイン
    port	ポート
    private	個人的
    quiet	無言
    quit	終了
    reconnecting	再接続
    refresh	リフレッシュ
    register	記録
    reject	拒絶
    script	スクリプト
    secret	秘密
    send	送
    server	サーバー
    servers	サーバー
    service	サービス
    services	サービス
    set		セット
    shutdown	閉鎖
    signoff	サインオッフ
    sound	音
    speak	話
    text	テキスト
    time	時間
    topic	話題
    unban	Unban
    user	ユーザー
    users	ユーザー
    view	観察
    who		誰
    whois	今誰
    whowas	誰でした
    windows	ヴィンドーズ
}
#
# Compounds of the above that are no language dependent
#
array set ztrans "
    bankick	{$ztrans(ban)+$ztrans(kick)}
"
