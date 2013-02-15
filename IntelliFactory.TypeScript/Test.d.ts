interface Selfish {
  foo(x: Selfish) : number;
}

interface FlashHidingArgs
{
  state?: string;
  elem?: Element;
}

interface InitOptions
{
  appId?: string;
  cookie?: bool;
  logging?: bool;
  status?: bool;
  xfbml?: bool;
  channlUrl?: string;
  authResponse?: any;
  frictionlessRequests?: bool;
  hideFlashCallback?: (args: FlashHidingArgs) => void;
}

interface AuthResponse
{
  accessToken: string;
  expiresIn: string;
  signedRequest: string;
  userId: string;
}

interface LoginResponse
{
  authResponse: AuthResponse;
  status: string;
}

interface LoginOptions
{
  display?: string;
  scope?: string;
}

interface Facebook
{
  api(url: string, method?: string, options?: any, callback?: (result: any) => void) : void;
  getAuthResponse() : AuthResponse;
  getLoginStatus(k: (response: LoginResponse) => void) : void;
  init(opts?: InitOptions) : void;
  login(onResponse: (response: LoginResponse) => void, options?: LoginOptions) : void;
  logout(onResponse: (response: LoginResponse) => void) : void;
}

var FB : Facebook;
