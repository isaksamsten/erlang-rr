function model = rflearn(X, y, opts = '-n 100 --progress none', quiet = 1)
%
% -- Function File:  rflearn(X, y, OPTIONS, quiet = 1)
%
%    Learn a random forest model from the examples in X with the targets in y
%    multiple targets are allowed (i.e. |y| >= 2). Set quiet to FALSE to ignore
%    output to standard error
%
%    OPTIONS:
%    -c, --cores         Number of cores used by the algorightm for
%                        constructing the model. [default: 4]
%    --progress          Show a progress bar while building a model.
%                        Available options include: 'dots', 'numeric' and
%                        'none'.  [default: dots]
%    --score             Defines the measure, which should be minimized, for
%                        evaluating the goodness of split points in each
%                        branch. Available options include: 'info', 'gini'
%                        and 'gini-info', where 'info' denotes information
%                        entropy and 'gini' the gini-impurity. [default: info]
%    --rule-score        Defines the measure, which should be minimized, for
%                        evaluating the goodness of a specific rule.
%                        Available otpions include: 'm', 'laplace' and
%                        'purity', where 'm' denotes the m-estimate [default:
%                        laplace]
%    -n, --no-trees      Defines the number of classifiers (trees) to build.
%                       [default: 10]
%    --max-depth         Defines the maximum allowed depth for a single
%                        decision tree. [default: 1000]
%    --min-examples      Min number of examples allowed for splitting a node
%                        [default: 1]
%    --feature-sampling  Select a method for feature sampling. Available
%                        options include: 'subset', 'rule', 'random-rule',
%                        'resample', 'weka', and 'combination'. [default:
%                        subset]
%    -m, --missing       Distributing missing values according to different
%                        strategies. Available options include: 'random',
%                        'randomw', 'partitionw', 'partition', 'weighted',
%                        'left', 'right' and 'ignore'. If 'random' is used,
%                        each example with missing values have an equal
%                        probability of be distributed over the left and
%                        right branch. If 'randomw' is selected, examples are
%                        randomly distributed over the left and right branch,
%                        but weighted towards the majority branch. If
%                        'partition' is selected, each example is distributed
%                        equally over each branch. If 'partitionw' is
%                        selected, the example are distributed over each
%                        branch but weighted towards the majority branch. If
%                        'weighted' is selected, each example is distributed
%                        over the majority branch. If 'left', 'right' or
%                        'ignore' is selected, examples are distributed
%                        either to the left, right or is ignored,
%                        respectively. [default: weighted]
%    -d, --distribute    Distribute examples at each split according to
%                        different strategies. Available option include:
%                        'default' or 'rulew'. If 'default' is selected,
%                        examples are distributed to either left or right. If
%                        'rulew' is selected, fractions of each example are
%                        distributed according to how many antecedents each
%                       rule-node classifies the example. [default: default]
%    --example-sampling  Select the method for feature sampling. Available
%                        options include: 'bagging' and 'subagging'.
%                        [default: bagging]
%    --variance-options  Options for the triangle variance sampling method.
%                        Format: 'Threshold A B C' [default: 0.3,1,100,10]
%    --weight-factor     Used for controlling the randomness of the
%                        'combination' and 'weighted'-arguments. [default:
%                        0.5]
%    --no-resample       Number of re-samples. [default: 6]
%    --min-gain          Minimum allowed gain for not re-sampling (if the
%                        'resample'-argument is specified). [default: 0]
%    --no-features       Number of features to inspect at each split. If set
%                        to log log(F)+1, where F denotes the total number of
%                        features, are inspected. The default value is
%                        usually a good compromise between diversity and
%                        performance. [default: default]
%    --no-rules          Number of rules to generate (from n features,
%                        determined by 'no-features'). Options include:
%                        'default', then 'no-features' div 2, 'same', then
%                        'no-features' is used otherwise n is used. [default:
%                        ss]
%
%    Isak Karlsson <isak-kar@dsv.su.se> (last edit: dec 2013)
%
%    See also: rfpredict
        tmpfile = rfdataset(X, y);
        model.tmpnam = tmpnam('./tmp/', 'model');

        if quiet
                q = ' 2> /dev/null';
        else
                q = '';
        end
        [status, ~]     = system(sprintf('./rr deploy -i %s -c "rf %s" -o %s %s', tmpfile, opts, model.tmpnam, q));
        model.nfeatures = size(X,2);
        model.nexamples = size(X,1);
        model.opts = opts;
        model.vi = variable_importance(model.tmpnam, q, model.nfeatures);
        model.oob_error = 0;
end

function vi = variable_importance(tmpnam, q, nfeatures)
        [status, vari] = system(sprintf('./rr vi -m %s %s', tmpnam, q));
        list = strread(vari, '%s', 'delimiter', sprintf('\n'));
        vi = zeros(nfeatures, 1);
        for i=1:length(list)
          [s, e, te, m, t] = regexp(list(i), '(.+):(.+)');
                idx = str2double(t{1}{1}{1,1});
                val = str2double(t{1}{1}{1,2});
                vi(idx) = val;
        end
end
