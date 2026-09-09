function prstack --wraps prstack --description 'prstack with Bitbucket credentials from the macOS Keychain'
    # Only exported for this call, so other programs never see the token.
    # Store the credentials with:
    #   security add-generic-password -a $USER -s prstack-bitbucket-email -w
    #   security add-generic-password -a $USER -s prstack-bitbucket-api-token -w
    if set -q BITBUCKET_API_TOKEN
        command prstack $argv
        return
    end
    set -l email (security find-generic-password -a $USER -s prstack-bitbucket-email -w 2>/dev/null)
    set -l token (security find-generic-password -a $USER -s prstack-bitbucket-api-token -w 2>/dev/null)
    if test -z "$email"; or test -z "$token"
        command prstack $argv
    else
        BITBUCKET_EMAIL=$email BITBUCKET_API_TOKEN=$token command prstack $argv
    end
end
