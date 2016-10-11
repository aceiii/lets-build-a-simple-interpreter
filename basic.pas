BEGIN
    BEGIN
        number := 2;
        a := number;
        b := 10 * a + 10 * number div 4;
        c := a - - b;
        _blah := 99
    END;
    _blah := 123;

    x := 11;
END.

