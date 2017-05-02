
#define K_DIR_ARG    "--dir="
#define K_OUT_ARG    "--out="
#define K_VERBOSE_ARG    "--verbose"
#define K_LIST_ARG    "--list="
#define K_INGOREREAD_ARG    "--ignore-read-error"

MEMVAR dirs,schemaDir,verbose,prefixes,indexes,ignoreReadError

PROCEDURE CreateStructDirIfMissing(schemaDir)
    LOCAL path := schemaDir
    IF(!FILE(path))
        MakeDir(schemaDir)
    ENDIF

FUNCTION CheckArgFlag (arg, flag)
    return Lower(Left(arg, Len(flag))) == flag

FUNCTION ParseFlag (arg, flag)
    return SubStr(arg, Len(flag) + 1)

PROCEDURE ParseArguments(args)
    local tmp, preFilePath

    PUBLIC dirs := {}
    PUBLIC schemaDir := "schema"
    PUBLIC verbose := .F.    
    PUBLIC prefixes := {}
    PUBLIC indexes := {{".NT*", "DBFNTX"}/*,{".ND*", "DBFNDX"}, {".CD*", "DBFCDX"},{".MD*","DBFMDX"}*/}
    PUBLIC ignoreReadError := .F.

    FOR EACH tmp IN args
        DO CASE
        CASE CheckArgFlag(tmp, K_DIR_ARG)
            AAdd(dirs, ParseFlag(tmp,K_DIR_ARG))
        CASE CheckArgFlag(tmp,K_OUT_ARG)
            schemaDir := ParseFlag(tmp,K_OUT_ARG)
        CASE CheckArgFlag(tmp,K_VERBOSE_ARG)
            verbose := .T.
            OutStd("Verbose", HB_EOL())
        CASE CheckArgFlag(tmp,K_INGOREREAD_ARG)
            ignoreReadError := .T.
        CASE CheckArgFlag(tmp, K_LIST_ARG)
            preFilePath := ParseFlag(tmp, K_LIST_ARG)
            HB_FUse(preFilePath)
            DO WHILE !HB_FEOF()
                AAdd(prefixes, ALLTRIM(HB_FReadAndSkip()))
            ENDDO
            HB_FUse()
        OTHERWISE
            AAdd(prefixes, tmp)
        ENDCASE
    NEXT

    if EMPTY(dirs)
      AAdd(dirs, "")
    end if

    if verbose
        AEval(dirs, {|d| OutStd("Dir:" , d, HB_EOL())} )
        AEval(prefixes, {|p| OutStd( "Prefix:" , p, HB_EOL())} )
        OutStd("Ignore Read errors:", ignoreReadError, HB_EOL())
    endif
    RETURN

FUNCTION SetupMapOfHashes()
    LOCAL priorFiles, fileData, filePath, fileSha, pfile
    LOCAL mapOfHashes := hb_Hash()

    hb_hSetAutoAdd(mapOfHashes, .T.)

    priorFiles := DIRECTORY(schemaDir + HB_PS())
    for each pfile in priorFiles
        fileData := ""
        filePath := schemaDir + HB_PS() + pFile[1]
        HB_FUse(filePath)
            DO WHILE !HB_FEOF()
                fileData += HB_FReadAndSkip()
            ENDDO
        HB_FUse()
        fileSha := HB_StrToHex(hb_sha256(fileData))
        mapOfHashes[filePath] := {fileSha, ""}
    next
    return mapOfHashes

PROCEDURE UpdateMapOfHashes(mapOfHashes)
    LOCAL priorFiles, fileData, filePath, fileSha, pfile, priorSha
    priorFiles := DIRECTORY(schemaDir + HB_PS())
    for each pfile in priorFiles
        fileData := ""
        filePath := schemaDir + HB_PS() + pFile[1]
        HB_FUse(filePath)
            DO WHILE !HB_FEOF()
                fileData += HB_FReadAndSkip()
            ENDDO
        HB_FUse()
        fileSha := HB_StrToHex(hb_sha256(fileData))
        if !hb_hHaskey(mapOfHashes, filePath)
            mapOfHashes[filePath] := {"", fileSha}
        else
            priorSha := mapOfHashes[filePath][1]
            mapOfHashes[filePath] := {priorSha, fileSha}
        endif 

    next

    RETURN

PROCEDURE WriteOutIndexSchema(prefixDir, missingExt, db)
    LOCAL suffix, look, file, fn, files, ikey, idx, hnd
    for each suffix in indexes
        look := prefixDir + missingExt + suffix[1]
        files := DIRECTORY(look)
        for each file in files
            fn :=  file[1]
            idx := prefixDir + fn
            if verbose
                OutStd("Index:", idx, HB_EOL())
            endif
            USE (db) READONLY INDEX (idx) VIA suffix[2]
            ikey := INDEXKEY()
            hnd := FCREATE(schemaDir + HB_PS() + fn + ".txt")
            FWRITE(hnd, ikey)
            FWRITE(hnd, HB_EOL())
            FCLOSE(hnd)
        next
    next
    RETURN

PROCEDURE WriteOutFileSchema(fn, db)
    local hnd, fld, afields
    if verbose
        OutStd("DB:", db, HB_EOL())
    endif
    USE (db) READONLY
    IF NETERR()
        Break "DBF Open Error"
    ENDIF
    afields := DBSTRUCT()
    hnd := FCREATE(schemaDir+ HB_PS() + fn + ".txt")
    for each fld in afields
        FWRITE(hnd, fld[1])
        FWRITE(hnd, CHR(9))
        FWRITE(hnd, fld[2])
        FWRITE(hnd, CHR(9))
        FWRITE(hnd,STR(fld[3]))
        FWRITE(hnd, CHR(9))
        FWRITE(hnd,STR(fld[4]))
        FWRITE(hnd, CHR(9))
        FWRITE(hnd, HB_EOL())
    next
    FCLOSE(hnd)
    RETURN

FUNCTION WriteOutSchema()

    LOCAL fileCount, look, file, dir, prefix, prefixDir, files, missingExt, db, fn
    LOCAL readError := .F.
    fileCount := 0
    for each dir in dirs
        if verbose
                OutStd("Checking:", HB_PathNormalize( HB_PS() + CurDir() + HB_PS() + dir), HB_EOL())
        endif
        for each prefix in prefixes
            look := dir + prefix + ".DBF"
            if verbose
                OutStd("Prefix: ", look, HB_EOL())
            endif
            prefixDir = dir
            HB_FNameSplit(look, @prefixDir)
            files := DIRECTORY(look)
            for each file in files
              fileCount += 1
              fn := file [1]
              db := prefixDir + fn
              missingExt := LEFT(fn, len(fn) -4)

              BEGIN SEQUENCE
              WriteOutFileSchema(fn, db)
              WriteOutIndexSchema(prefixDir, missingExt, db)
              RECOVER
                 OutStd("Could Not Access: ", fn, HB_EOL())
                 readError := .T. .AND. .NOT. ignoreReadError
              END
            next
        next
    next
    IF fileCount == 0
        OutErr("Didn't find any files to check schema.", HB_EOL())
    end

    RETURN .NOT. readError

PROCEDURE MAIN(...)
    LOCAL args := HB_AParams()
    LOCAL key, schemaParseOk
    LOCAL test
    LOCAL mapOfHashes

    ParseArguments(args)

    CreateStructDirIfMissing(schemaDir)

    mapOfHashes := SetupMapOfHashes() 
    
    schemaParseOk := WriteOutSchema()

    UpdateMapOfHashes(mapOfHashes)

  
    test := .T.
    for each key in hb_hKeys(mapOfHashes)
       if mapOfHashes[key][1] <> mapOfHashes[key][2] 
            test := .F.
            OutErr(key, " schema has changed.", HB_EOL())
       endif
    next

    if !test 
         OutErr("Please recommit", HB_EOL())
         ERRORLEVEL(25)
    ENDIF

    if !schemaParseOk
         ERRORLEVEL(24)
    ENDIF
 
    quit
