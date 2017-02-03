namespace TimeEntry

module Authorization =
    
    open TimeEntry.DomainTypes
    open TimeEntry.Constructors


    let onlyForSameLogin (login: Login ) (userinfo: UserInfo) (f: Login -> 'a) = 
        if userinfo.Login = login then
            Some f
        else
            None

    let onlyForAdmin (user: UserInfo) (f: 'a -> 'b) = 
        if user.Level = Admin then 
            Some f
        else
            None 

    let onlyForAuthSites (site: Site) (userinfo: UserInfo) (f: Site -> 'a) = 
        match userinfo.SiteAccess with
            | AllSites -> Some f
            | SiteList sites ->
                if List.contains site sites then
                    Some f
                else None
