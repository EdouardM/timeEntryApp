namespace TimeEntry

module Authorization =
    
    open TimeEntry.DomainTypes
    open TimeEntry.Constructors


    let onlyForSameLogin (login: Login ) (user: UserInfo) (f:Login -> 'a) = 
        if user.Login = login then
            Some (fun () -> f login)
        else
            None

    let onlyForAdmin (login: Login) (user: UserInfo) (f: Login -> 'a) = 
        if user.Level = Admin then 
            Some ( fun () -> f login )
        else
            None 

    let passwordUpdate (login: Login ) (user: UserInfo) (f:Login * Password -> 'a) = 
        if user.Login = login then 
            Some ( fun password -> f (login, password) )
        else
            None 


    let userNameUpdate (login: Login ) (user: UserInfo) (f:Login * UserName -> 'a) = 
        if user.Login = login then 
            Some ( fun username -> f (login, username) )
        else
            None 

