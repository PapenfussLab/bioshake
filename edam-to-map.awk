BEGIN{
    FPAT="([^,]*)|(\"[^\"]*\")"
}


function camelcase(str, i, n, name)
{
    n = split(str, words, /[^a-zA-Z0-9]/)
    name = ""
    for(i = 1; i <= n; i++){
        name = name toupper(substr(words[i],1,1)) tolower(substr(words[i],2))
    }
    return name
}

$5 == "FALSE"{
    name = camelcase($2)
    if(substr(name,1,1) + 0 != 0)
        next
    if(name == "")
        next
    if(!printed[name]){
        print "class " name " x"
        printed[name]++
    }
    id[$1] = name
    if($15 != "")
        ext[$1] = $15

    n = split($8, words, /\|/)
    for(i = 1; i <= n; i++){
        parents[name][++cnt[name]] = words[i]
    }
}

END{
    print ""
    print "edam :: M.Map T.Name EDAMNode"
    printf("edam = M.fromList [")
    for(i in id) {
        n = id[i]
        if(done[n])
          continue
        done[n]++


        printf("%s(''%s, EDAMNode \"%s\"", comma?"  ,":"", n, i)
        comma++
        if(ext[i]){
            m = split(ext[i], exts, /\|/)
            printf(" (Just [\"%s\"", exts[1])
            for(j = 2; j <= m; j++)
                printf(",\"%s\"", exts[j])
            printf("])")
        } else
            printf(" Nothing")

        printf(" [")
        k = 0;
        for(j = 1; j <= cnt[n]; j++)
            if(id[parents[n][j]] != ""){
                printf("%s''%s", k?", ":"", id[parents[n][j]])
                k++
            }
        printf("])\n")
    }
    printf("  ]")
}
